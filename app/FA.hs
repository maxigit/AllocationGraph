{-# LANGUAGE OverloadedStrings #-}
import BasePrelude
import AllocationGraph
import AllocationGraph.Diagrams
import Diagrams.Backend.SVG.CmdLine (mainWith)

import Database.MySQL.Simple as SQL
import Database.MySQL.Simple.QueryResults

import Data.String
import qualified Data.Map as Map
import qualified Data.Map (Map)

import qualified Data.Colour.Palette.BrewerSet as K


data Options = Options
  { faCredential :: !(String) -- ^ database credentials. Read format
  }
main :: IO()
main = do 
  (supp:args) <- getArgs
  let supplier_id = read supp
  credentials <- read <$> readFile "credentials.cfg"
  conn <- SQL.connect credentials
  (resources, allocations) <- loadAllocations conn supplier_id
  let param = RenderParameter width height width sourceColour
      diag = renderAllocation param _resType graph
      Right graph = buildGraph resources allocations
      width = 10
      height res box = width * log' where
             log' = logBase 5 (abs box + 1)

      -- We want to give a different colour for each source in 
      -- order they are displayed. For that we build a Map
      sources = filter isSource resources
      colours = cycle $ K.brewerSet K.Set2 8
      resourceToColour = Map.fromList $ zip sources colours
      sourceColour res = fromJust $ Map.lookup res resourceToColour
  withArgs args $ mainWith diag


supplierInvoice = 20
supplierCredit = 21
supplierPayment = 22
supplierDeposit = 25


type Key = (Int, Int)
loadAllocations :: SQL.Connection -> Int -> IO ([Resource Key], [Allocation Key])
loadAllocations conn supp = do
  let resourceQuery = fromString $ 
                      "SELECT trans_no, type, reference, supp_reference, DATE_FORMAT(tran_date, '%Y-%m-%d')"
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

  where toResource :: (Int, Int, String, String, String, Double) -> Resource Key
        toResource (no, t, ref, supp_ref, date, amount) = Resource (name t ref supp_ref date) (no,t) (resourceType t) amount
        resourceType t | t `elem` [supplierInvoice, supplierDeposit] = Target
                       | otherwise = Source

        name :: Int -> String -> String -> String -> String
        name t ref supp_ref date = printf ("%s-%s  [ %s ]  %s" :: String)
                                  (showType t) ref supp_ref date
          
        showType :: Int -> String
        showType 22 = "Payment"
        showType 21 = "Credit"
        showType 20 = "Invoice"
        showType 25 = "Deposit"

        toAlloc (no_from, type_from, no_to, type_to, amount) =
                Allocation amount (no_from, type_from) (no_to, type_to)

