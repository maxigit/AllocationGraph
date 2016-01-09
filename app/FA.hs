{-# LANGUAGE OverloadedStrings #-}
import BasePrelude hiding((&))
import Control.Lens
import AllocationGraph
import AllocationGraph.Diagrams
import Diagrams.Backend.SVG.CmdLine (mainWith)

import Database.MySQL.Simple as SQL
import Database.MySQL.Simple.QueryResults

import Data.String
import qualified Data.Map as Map
import qualified Data.Map (Map)

import qualified Data.Colour.Palette.BrewerSet as K
import Data.Time
import Data.Time.Format

data ScaleMode = Log | Linear deriving (Read, Show )
data Options = Options
  { faCredential :: !(String) -- ^ database credentials. Read format
  , scaleMode :: ScaleMode
  }


getDate resource = let
  n = _resName resource
  in drop (length n - 10) n
groupByYear res = let d = take 8 (getDate res)
                  in  Right $ res & resKey .~ d
                                  & resName .~ d

main :: IO()
main = do 
  args <- getArgs
  let (scaleMode, supplier_id, args') = case args of
        ("-l":supp:args) -> (Linear, read supp, args)
        (supp:args) -> (Log, read supp, args)
  credentials <- read <$> readFile "credentials.cfg"
  conn <- SQL.connect credentials
  (resources, allocations) <- loadAllocations conn supplier_id
  let param = RenderParameter width (height scaleMode) width allocColour
      diag = renderAllocation param _resType (orderTargets graph)
      Right graph' = buildGraph resources allocations
      graph = groupResources graph' groupByYear 
      width = 10
      height Log res box = width * log' where
             log' = max 1 (logBase 5 (abs box + 1))
      height Linear res box = box /10

      -- We want to give a different colour for each source in 
      -- order they are displayed. For that we build a Map
      targets = filter isTarget (_graphResources graph)
      colours = cycle $ K.brewerSet K.BrBG 11 --  K.Set2 8
      resourceToColour = Map.fromList $ zip targets colours
      allocColour alloc = fromJust $ Map.lookup (_allocTarget alloc) resourceToColour
  print graph
  withArgs args' $ mainWith diag


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
  print resourceQuery
  rows <- SQL.query_ conn resourceQuery
  let resources = map toResource rows
  mapM_ print resources

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
        , " ORDER by from_.tran_date, from_.trans_no " 
        ]

  print allocQuery
  rows' <- SQL.query_ conn allocQuery
  let types = rows' :: [(Int, Int, Int, Int, Double)]

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

        toAlloc (no_from, type_from, no_to, type_to, amount) =
                Allocation amount (no_from, type_from) (no_to, type_to)

formatDate :: Day -> String
formatDate = show --  formatTime defaultTimeLocale "%Y/%m/%d"
