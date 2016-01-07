import BasePrelude
import AllocationGraph
import AllocationGraph.Diagrams
import Diagrams.Backend.SVG.CmdLine (mainWith)

import Database.MySQL.Simple as SQL
import Database.MySQL.Simple.QueryResults


data Options = Options
  { faCredential :: !(String) -- ^ database credentials. Read format
  }
main :: IO()
main = do 
  credentials <- read <$> readFile "credential.cfg"
  conn <- SQL.connect credentials
  let (resources, allocations) = loadAllocations conn
      param = RenderParameter (const 50) (*5) 10
      diag = renderAllocation param _resType graph
      Right graph = buildGraph resources allocations

  mainWith diag



type Key = (Int, Int)
loadAllocations :: SQL.Connection -> ([Resource Key], [Allocation Key])
loadAllocations cred = undefined




