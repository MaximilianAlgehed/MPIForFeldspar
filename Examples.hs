import Prelude ()
import Feldspar.Run
import Feldspar.Data.Vector
import MPIForFeldspar

-- | A simple hello world
helloWorld :: Run ()
helloWorld = do
            (rank, size) <- setup
            on (1 :: Data Int32) (do
                                    arr <- listManifest [(polar (1 :: Data Double) 5), polar 1 2]
                                    send arr
                                       0
                                       0
                                       mpi_comm_world
                                 )
            on (0 :: Data Int32) (do
                                     ar <- recv 2 1 0 mpi_comm_world :: Run (DArr (Complex Double))
                                     a  <- getArr ar 0
                                     printf "I just got (%f, %f)!\n" (realPart a) (imagPart a)
                                 )
            finish
