{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module MPIForFeldspar where
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
          iff (fold1 (||) $ map (rank==) locations)
            r
            $ return ()

-- | Send a value to a node
send :: (MPITypeable a, Finite a, Syntax a, PrimType' (Internal a))
    => a
    -> Data Int32 -- Target
    -> Data Int32 -- Tag
    -> Communicator -- What Communicator to use
    -> Run ()
send a target tag comm =
    do
      r <- initRef a
      callProc "MPI_Send" [refArg r, valArg (length a), constArg "" (mpiType a), valArg target, valArg tag, comm]

-- | Recieve a value from somewhere
recv :: (MPITypeable a, Syntax a, PrimType' (Internal a))
    => Ref a
    -> Data Word32 -- Size
    -> Data Int32 -- From 
    -> Data Int32 -- Tag
    -> Communicator
    -> Run ()
recv r size target tag comm =
    do
        a <- getRef r
        callProc "MPI_Recv" [refArg r, valArg size, constArg "" (mpiType a), valArg target, valArg tag, comm, constArg "" "MPI_STATUS_IGNORE"]

-- | An example program
program :: Run ()
program = do
            (rank, size) <- setup
            printf "I'm process %d out of %d\n" rank size
            on (1 :: Data Int32) (send (5 :: Data Int32) 0 0 mpi_comm_world)
            on (0 :: Data Int32) (do
                                     r <- initRef (0 :: Data Int32)
                                     recv r 1 1 0 mpi_comm_world
                                     a <- getRef r
                                     printf "I just got %d\n" a
                                 )
            finish
