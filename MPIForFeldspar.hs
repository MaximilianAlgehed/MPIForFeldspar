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

-- | Default world communicator
mpi_comm_world :: Communicator
mpi_comm_world = constArg "" "MPI_COMM_WORLD"

-- | Set up MPI
setup  = do
          addInclude "<mpi.h>"
          callProc "MPI_Init" [valArg (0 :: Data Int32), valArg (0 :: Data Int32)]
          rank_ref <- initRef (0 :: Data Int32)
          size_ref <- initRef (0 :: Data Int32)
          callProc "MPI_Comm_rank" [constArg "" "MPI_COMM_WORLD", refArg rank_ref]
          callProc "MPI_Comm_size" [constArg "" "MPI_COMM_WORLD", refArg size_ref]
          rank <- getRef rank_ref
          size <- getRef size_ref
          return (rank, size)

-- | finalize mpi
finish = callProc "MPI_Finalize" []

-- | Run a computation on some group of nodes
on :: (DManifestable a Int32) => a -> Run ()-> Run ()
on as r = do
          rank_ref <- initRef (0 :: Data Int32)
          callProc "MPI_Comm_rank" [constArg "" "MPI_COMM_WORLD", refArg rank_ref]
          rank <- getRef rank_ref
          locations <- manifested as
          iff (fold (||) false $ map (rank==) locations)
            r
            $ return ()

-- | Send a value to a node
send :: (MPITypeable a, MPIReferable a, Sizeable a)
    => a            -- The data to send
    -> Data Int32   -- Target
    -> Data Int32   -- Tag
    -> Communicator -- What Communicator to use
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
    -> Data Int32   -- From 
    -> Data Int32   -- Tag
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
