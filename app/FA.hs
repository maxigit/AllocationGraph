{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
import BasePrelude hiding((&),(.),id, option)
import Control.Lens
import AllocationGraph
import AllocationGraph.Csv
import AllocationGraph.Diagrams
import Diagrams.Backend.SVG.CmdLine (mainWith)

import Database.MySQL.Simple as SQL

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Colour.Palette.BrewerSet as K
import Data.Time

import qualified Options.Applicative as OP
import Options.Applicative (Parser, strOption, long, short, metavar
                           , help, value, flag
                           , option, auto)

-- import Text.Regex
import Text.Regex.PCRE

data ScaleMode = Log | Linear deriving (Read, Show )
data OrderMode = ReorderTarget | OriginalOrder deriving (Show, Read)
data AllocWidthMode = ABoxHeight | AConst deriving (Show, Read)

data Options = Options
  { opFACredential :: !(String) -- ^ database credentials. Read format
  , opScaleMode :: ScaleMode
  , opSep :: Double
  , opBefore :: Maybe Day
  , opAfter :: Maybe Day
  , opGroupPeriod :: Maybe Period
  , opEntity :: Int
  , opOrderMode :: OrderMode
  , opAllocWidthMode :: AllocWidthMode
  , opArgs :: [String]
  }
  
data Period = Week | Month Int | Quarter Int Int | Year Int Int
     deriving (Show, Read) 

optionParser :: Parser Options
optionParser = pure Options
  <*> strOption ( long "credential"
                 <> short 'f'
                 <> metavar "FILE"
                 <> help "FrontAccounting DB credentials"
                 <> value "credentials.cfg"
                 )
  <*> flag Log Linear ( long "scale-mode"
                   <> short 'l'
             )
  <*>  option auto ( long "separator_width"
                    <> short 's'
                    <> metavar "SEP WIDTH"
                    <> help "Separator witdth"
                    <> value 0
                    )
  <*>  optional (fmap readDate $ strOption (long "before_date"
                             <> short 'b'
                             <> metavar "DATE"
                             <> help "Before date"
       ))
  <*>  optional (fmap readDate $ strOption (long "after_date"
                             <> short 'a'
                             <> metavar "DATE"
                             <> help "After date"
       ))
  <*> optional (fmap readPeriod $ strOption (long "group_period"
                                    <> short 'p'
                                    <> metavar "PERIOD .."
                                    <> help "Grouping period"
                                    ))
  <*>  option auto ( long "entity_id"
                   <> short 'e'
                   <> metavar "ENTITY ID"
                   <> help "entity id"
                   )
  <*>  flag ReorderTarget OriginalOrder
            ( long "order_mode"
            <> short 't'
            )
  <*> flag ABoxHeight AConst
           ( long "alloc_width_mode"
           <> short 'w'
           )
              
  <*> many (OP.argument OP.str (metavar "DIAGRAMS OPTIONS ..."))

readDate :: String -> Day
readDate str = let
  formats = ["%D", "%F"]
  attempts = map (parseTimeM True defaultTimeLocale) formats <*> [str]
  in head $ catMaybes (attempts
               ++ [error $ "Can't parse date : ["
                         ++ str ++ "]"
                  ]
               )

readPeriod :: String -> Period
readPeriod s = read s 
getDate :: Resource k e -> String
getDate resource = let
  n = _resName resource
  in drop (length n - 10) n
groupByYear :: ResourceFA -> Either ResourceFA (Resource String Extra)
groupByYear res = let d = take 8 (getDate res)
                  in  Right $ res & resKey .~ d
                                  & resName .~ d

getOptions :: IO Options
getOptions = OP.execParser opts where
  opts = OP.info (OP.helper <*> optionParser) OP.fullDesc

resourceToColumn res = let
 virtual =  map toLower (_resName res) =~ ("(-pre|mirror|rev)" :: String)
            || abs (_resAmount res) > 60000
 in case (_resType res, virtual) of
    (Source, False) -> 1
    (Target, True) -> 2
    (Source, True) -> 3
    (Target, False) -> 4

  

main :: IO()
main = do 
  options <- getOptions
  credentials <- read <$> readFile (opFACredential options)
  conn <- SQL.connect credentials
  (resources, allocations) <- loadAllocations conn (opEntity options)
  let diag = renderAllocation param
                              resourceToColumn -- _resType
                              graph
      Right graph' = buildGraph resources allocations
      graph'' = (case opOrderMode options of 
                      ReorderTarget -> orderTargets graph'
                      _ -> graph'
                )
      graph = groupResources graph'' (groupFunction options)
      param = RenderParameter width height width allocColour allocWidth
      width = 10
      height' Log _ box = width * log' where
             log' = max 1 (logBase 5 (abs box + 1))
      height' Linear _ box = box /10
      height = height'(opScaleMode options)
      allocWidth = case opAllocWidthMode options of
                        ABoxHeight -> \a -> height (_allocAmount a)
                                                   (_allocAmount a)
                        _ -> const width

      -- We want to give a different colour for each source in 
      -- order they are displayed. For that we build a Map
      targets = filter isTarget (_graphResources graph)
      colours = cycle $ K.brewerSet K.BrBG 11 --  K.Set2 8
      resourceToColour = Map.fromList $ zip targets colours
      allocColour alloc = fromJust $ Map.lookup (_allocTarget alloc) resourceToColour
  withArgs (opArgs options) $ do
            mapM_ putStrLn (renderCsv graph)
            mainWith diag


supplierInvoice, supplierCredit, supplierPayment, supplierDeposit :: Int
supplierInvoice = 20
supplierCredit = 21
supplierPayment = 22
supplierDeposit = 25


type Key = (Int, Int)
type Extra = Day
type ResourceFA = Resource Key Extra
loadAllocations :: SQL.Connection -> Int -> IO ([ResourceFA], [Allocation Key])
loadAllocations conn supp = do
  let resourceQuery = fromString $ 
                      "SELECT trans_no, type, reference, supp_reference, tran_date"
                      ++ " , ov_amount+ov_discount+ov_gst"
                      ++ " FROM 0_supp_trans WHERE "
                      ++ whereC "" 
                      ++ " ORDER BY tran_date, trans_no"
      whereC table = table ++ "type IN " ++ (show (supplierInvoice, supplierCredit, supplierPayment, supplierDeposit))
                          ++ " AND " ++ table ++ "supplier_id = " ++ (show supp)
  -- print resourceQuery
  rows <- SQL.query_ conn resourceQuery
  let resources = map toResource rows
  -- mapM_ print resources

  let allocQuery = fromString $ concat
        [ " SELECT trans_no_from, trans_type_from, trans_no_to, trans_type_to, amt "
        , " FROM 0_supp_allocations" 
        , " JOIN 0_supp_trans from_ ON (from_.trans_no = trans_no_from AND from_.type = trans_type_from) "
        , " JOIN 0_supp_trans to_ ON (to_.trans_no = trans_no_to AND to_.type = trans_type_to) "
        , " WHERE (" 
        , whereC "from_." 
        , ") AND (" 
        , whereC "to_." 
        , ")"
        , " AND abs(amt) >1e-2 "
        , " ORDER by from_.tran_date, from_.trans_no " 
        ]

  -- print allocQuery
  rows' <- SQL.query_ conn allocQuery
  -- let types = rows' :: [(Int, Int, Int, Int, Double)]

  let allocs = map toAlloc rows'
  mapM_ print allocs

  let resources' = take 50000 resources
      allocs' = filterAllocations resources' allocs
  -- return (resources, allocs)
  return (resources', allocs')

  where toResource :: (Int, Int, String, String, Day, Double) -> ResourceFA
        toResource (no, t, ref, supp_ref, date, amount) = Resource (name t ref supp_ref date) (no,t) (resourceType t) amount date
        resourceType t | t `elem` [supplierInvoice, supplierDeposit] = Target
                       | otherwise = Source

        name :: Int -> String -> String -> Day -> String
        name t ref supp_ref date = printf ("%s-%s  [ %s ]  %s" :: String)
                                  (showType t) ref supp_ref (formatDate date)
          
        showType :: Int -> String
        showType 22 = "Payment"
        showType 21 = "Credit"
        showType 20 = "Invoice"
        showType 25 = "Deposit"
        showType i = error "TransType [" ++ show i ++ "] not known."

        toAlloc (no_from, type_from, no_to, type_to, amount) =
                Allocation amount (no_from, type_from) (no_to, type_to)

formatDate :: Day -> String
formatDate = show --  formatTime defaultTimeLocale "%Y/%m/%d"


groupFunction :: Options
              -> ResourceFA
              -> (Either ResourceFA (Resource PeriodKey Extra))
groupFunction opt res = case groupsM of
                             [] -> Left res
                             (g:_) -> Right g
  where
    groupsM = catMaybes
      [ opBefore opt >>= groupBeforeStart  res
      , opAfter opt >>= groupBeforeEnd res
      , opGroupPeriod opt >>= groupByPeriod res
      ]

data PeriodKey = Before
               | WeekKey Integer Int
               | MonthKey Integer Int
               | QuarterKey Integer Int
               | YearKey Integer
               | After deriving(Show, Read, Ord, Eq)

groupBeforeStart :: ResourceFA -> Day -> Maybe (Resource PeriodKey Extra)
groupBeforeStart res before =  do
  guard (res ^. resExtra <= before)
  return ( Resource ( "<= " ++ formatDate before ) 
                     Before
                     (_resType res)
                     0
                     (res ^. resExtra)
          )
groupBeforeEnd :: ResourceFA -> Day -> Maybe (Resource PeriodKey Extra)
groupBeforeEnd res after =  do
  guard (res ^. resExtra >= after)
  return ( Resource ( ">= " ++ formatDate after ) 
                     After
                     (_resType res)
                     0
  
                     (res ^. resExtra)
          )

groupByPeriod :: ResourceFA -> Period -> Maybe (Resource PeriodKey Extra)
groupByPeriod res period  =
  Just $ res & resKey .~ periodKey
             & resName .~ periodKeyToName periodKey
  where
    periodKey = groupDate ( _resExtra res)
    groupDate date =
        let (year, month, day) = toGregorian date
            days = fromInteger $ diffDays date (fromGregorian year 01 01)
            week = days `div` 7
        in case period of
                Week -> WeekKey year week
                Month day1 -> let (y,m,_) = normalizeDate year month (day-day1)
                             in MonthKey y m
                Quarter month1 day1  ->  let (y,m,_) = normalizeDate year (month-month1) (day-day1)
                                         in QuarterKey y (1+(m `div` 3))
                Year month1 day1  ->  let (y,m,_) = normalizeDate year (month-month1) (day-day1)
                                      in YearKey y 


normalizeDate year month day = 
  adjustMonth $ adjustDay (year,month, day)
  where
    adjustDay (y,m,d) = if d <0
                           then (y,m-1,0)
                           else (y,m,d)
    adjustMonth (y,m,d) = if m<0
                             then (y-1,12+m,d)
                             else (y,m,d)
                            
                
periodKeyToName :: PeriodKey -> String
periodKeyToName Before = "BEFORE"
periodKeyToName (WeekKey y w) = printf "%04d-W%02d" y w
periodKeyToName (MonthKey y m) = printf "%04d-%02d" y m
periodKeyToName (QuarterKey y q) = printf "%04d-Q%1d" y q
periodKeyToName (YearKey y ) = printf "%04d-%04d" y (y+1)
periodKeyToName After = "AFTER"
  
