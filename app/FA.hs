{-# LANGUAGE OverloadedStrings #-}
import BasePrelude
import AllocationGraph
import AllocationGraph.Diagrams
import Diagrams.Backend.SVG.CmdLine (mainWith)

import Database.MySQL.Simple as SQL
import Database.MySQL.Simple.QueryResults

import Data.String


data Options = Options
  { faCredential :: !(String) -- ^ database credentials. Read format
  }
main :: IO()
main = do 
  let supplier_id = 2
  credentials <- read <$> readFile "credentials.cfg"
  conn <- SQL.connect credentials
  (resources, allocations) <- loadAllocations conn supplier_id
  let param = RenderParameter (const 50) (/50) 10
      diag = renderAllocation param _resType graph
      Right graph = buildGraph resources allocations

  mainWith diag


supplierInvoice = 20
supplierCredit = 21
supplierPayment = 22
supplierDeposit = 25


type Key = (Int, Int)
loadAllocations :: SQL.Connection -> Int -> IO ([Resource Key], [Allocation Key])
loadAllocations conn supp = do
  let resourceQuery = fromString $  "SELECT trans_no, type, supp_reference, ov_amount FROM 0_supp_trans WHERE "
                      ++ whereC "" 
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
  return (resources, allocs)

  where toResource (no, t, ref, amount) = Resource ref (no,t) (resourceType t) amount
        resourceType t | t `elem` [supplierInvoice, supplierDeposit] = Target
                       | otherwise = Source
        toAlloc (no_from, type_from, no_to, type_to, amount) =
                Allocation amount (no_from, type_from) (no_to, type_to)





