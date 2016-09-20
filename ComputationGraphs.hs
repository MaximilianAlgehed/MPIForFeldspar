module ComputationGraphs where
import Feldspar.Run
import Data.Graph.Inductive as G hiding (Node, Edge)
import qualified MPIForFeldspar as MPI

data Node a = Node {computation :: a -> Run a,
                    allocation  :: Maybe MPI.Node 
                   }

data Edge a = Edge {size :: Data Length}

data CGraph a = CGraph {graph :: Gr (Node a) (Edge a)}
