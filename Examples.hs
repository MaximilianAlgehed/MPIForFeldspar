import Prelude ()
import Feldspar.Run
import Feldspar.Data.Vector
import MPIForFeldspar

-- | A simple hello world
helloWorld :: Run ()
helloWorld = do
            (rank, size) <- setup
            printf "I'm process %d out of %d\n" rank size
            on (1 :: Data Int32) (do
                                    arr <- listManifest [1 :: Data Int32, 3]
                                    send arr (0 :: Data Int32) (0 :: Data Int32) mpi_comm_world)
            on (0 :: Data Int32) (do
                                     ar <- recv 2 1 0 mpi_comm_world :: Run (DArr Int32)
                                     a  <- getArr ar 0
                                     b  <- getArr ar 1
                                     printf "I just got %d and %d!\n" a b
                                 )
            finish
