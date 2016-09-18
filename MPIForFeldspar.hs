{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ScopedTypeVariables #-}
module MPIForFeldspar where
import Feldspar.Representation
import Typeclasses
import qualified Prelude as P
import Feldspar.Data.Vector
import Feldspar.Run

type Communicator = FunArg Data PrimType'
type Node         = Data Word32

root :: Node
root = 0

getRank :: Communicator -> Run (Data Word32)
getRank comm = do
            rank_ref <- initRef (0 :: Data Word32)
            callProc "MPI_Comm_rank" [comm, refArg rank_ref]
            getRef rank_ref

getSize :: Communicator -> Run (Data Word32)
getSize comm = do
            size_ref <- initRef (0 :: Data Word32)
            callProc "MPI_Comm_size" [comm, refArg size_ref]
            getRef size_ref

-- | Default world communicator
mpi_comm_world :: Communicator
mpi_comm_world = constArg "" "MPI_COMM_WORLD"

-- | Set up MPI
setup = do
          addInclude "<mpi.h>"
          callProc "MPI_Init" [valArg (0 :: Data Word32), valArg (0 :: Data Word32)]
          rank <- getRank mpi_comm_world
          size <- getSize mpi_comm_world
          return (rank, size)

-- | finalize mpi
finish = callProc "MPI_Finalize" []

-- | Run a computation on some group of nodes
on :: (DManifestable a Word32) => a -> Communicator -> Run ()-> Run ()
on as comm r = do
          rank <- getRank comm
          locations <- manifested as
          iff (fold (||) false $ map (rank==) locations)
            r
            $ return ()

-- | Run on all nodes but some group of nodes
onAllBut :: (DManifestable a Word32) => a -> Communicator -> Run ()-> Run ()
onAllBut as comm r = do
          rank <- getRank comm
          locations <- manifested as
          iff (not (fold (||) false $ map (rank==) locations))
            r
            $ return ()

-- | Send a value to a node
send :: (MPITypeable a, MPIReferable a, Sizeable a)
    => a             -- The data to send
    -> Node          -- Target
    -> Data Word32   -- Tag
    -> Communicator  -- What Communicator to use
    -> Run ()
send a target tag comm =
    do
      r <- refer a
      callProc "MPI_Send" [r,
                           valArg (size a),
                           constArg "" (mpiType a),
                           valArg target,
                           valArg tag,
                           comm]

-- | Recieve a value from somewhere
recv :: forall a. (Syntactic a, MPITypeable a, Syntax a, PrimType' (Internal a), Sizeable a)
    => Data Word32  -- Message size
    -> Node         -- From 
    -> Data Word32  -- Tag
    -> Communicator -- The communicator to use
    -> Run (Arr a)
recv n target tag comm =
    do
        arr <- newArr n 
        callProc "MPI_Recv" [arrArg arr,
                             valArg (n * (size (example :: a))),
                             constArg "" (mpiType (example :: a)),
                             valArg target,
                             valArg tag,
                             comm,
                             constArg "" "MPI_STATUS_IGNORE"]
        return arr

-- | Broadcast data from one process to all others
bcast :: (PrimType' (Internal a), MPITypeable a, Sizeable a, Syntax a)
      => Arr a
      -> Data Length
      -> Node
      -> Communicator
      -> Run ()
bcast arr n node comm = 
    do
        a <- getArr arr 0
        callProc "MPI_Bcast" [arrArg arr,
                              valArg (n * (size a)),
                              constArg "" (mpiType a),
                              valArg node,
                              comm]

-- | Send data from several processes to one
gatherSend :: forall a. (PrimType' (Internal a), MPITypeable a, Sizeable (Arr a), Sizeable a, Syntax a)
           => Arr a
           -> Node
           -> Communicator
           -> Run ()
gatherSend arr node comm =
    do
        callProc "MPI_Gather" [arrArg arr,
                               valArg (size arr),
                               constArg "" (mpiType (undefined :: a)),
                               valArg (0 :: Data Word32),
                               valArg (0 :: Data Word32),
                               constArg "" (mpiType (undefined :: a)),
                               valArg node,
                               comm]

-- | Gather data from the other processes
gatherRecv :: forall a. (PrimType' (Internal a), MPITypeable a, Sizeable (Arr a), Syntax a)
           => Arr a
           -> Arr a
           -> Communicator
           -> Run ()
gatherRecv arr arrS comm =
    do
        rank <- getRank comm
        callProc "MPI_Gather" [arrArg arrS,
                               valArg (size arrS),
                               constArg "" (mpiType (undefined :: a)),
                               arrArg arr,
                               valArg (size arrS),
                               constArg "" (mpiType (undefined :: a)),
                               valArg rank,
                               comm]

-- | Split a communicator
commSplit :: Data Word32  -- Colour 
          -> Data Word32  -- Key
          -> Communicator -- Communicator to split
          -> Run Communicator
commSplit colour key comm = do
    comm' <- newObject "MPI_Comm" False
    let arg = objArg comm'
    callProc "MPI_Comm_split" [comm, valArg colour, valArg key, addr arg]
    return $ arg

-- | Freeing a communicator
commFree :: Communicator -> Run ()
commFree comm = callProc "mpi_Comm_free" [addr comm]
