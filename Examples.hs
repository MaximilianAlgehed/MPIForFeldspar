{-# LANGUAGE FlexibleContexts #-}
import Prelude ()
import Feldspar.Run
import Feldspar.Data.Vector
import MPIForFeldspar

-- | A simple hello world
helloWorld :: Run ()
helloWorld = do
            (rank, size) <- setup
            on (1 :: Data Word32) mpi_comm_world
                                  (do
                                    arr <- listManifest [(polar (1 :: Data Double) 5), polar 1 2]
                                    send arr
                                       0
                                       0
                                       mpi_comm_world
                                  )
            on (0 :: Data Word32) mpi_comm_world
                                  (do
                                     ar <- recv 2 1 0 mpi_comm_world :: Run (DArr (Complex Double))
                                     a  <- getArr ar 0
                                     printf "I just got (%f, %f)!\n" (realPart a) (imagPart a)
                                  )
            finish

-- | Discrete Fourier Transform
dft :: (RealFloat a, PrimType a, PrimType (Complex a))
    => Data Length
    -> Data Length
    -> DPull (Complex a)
    -> DPull (Complex a)
dft start len vec = fromColVec $ matMul (mat (length vec)) (toColVec vec)
       where
           mat n = Pull2 len n $ \k l ->
                   polar 1 (-(2*π * i2n (k + start) * i2n l) / i2n n)

-- | A simple program to demonstrate broadcasts and gathers
broadcastGather :: Run ()
broadcastGather = do
            (rank, size) <- setup

            dsize <- newArr 1 :: Run (DArr Word32)
            on root mpi_comm_world (do
                        input <- readStd :: Run (Data Length)
                        setArr dsize 0 input 
                    )
            bcast dsize 1 0 mpi_comm_world

            -- Initialize the data array for when we get the data from the root node
            arrSize <- getArr dsize 0
            dataArr <- newArr arrSize :: Run (Arr (Data Float))
            on root mpi_comm_world $ do
                        -- Initialize the array, pretend to "read it from the user"
                        result <- manifest dataArr $ map i2n (0...(arrSize - 1)) 
                        result <- thawArr result
                        bcast result arrSize root mpi_comm_world

            -- send the array over the wire
            onAllBut root mpi_comm_world $ bcast dataArr arrSize root mpi_comm_world

            -- calculate the DFT
            let chunkSize = arrSize `div` size
            manifestedDArr <- freezeArr dataArr
            resarr <- newArr chunkSize :: Run (DArr (Complex Float))
            resultV <- manifest resarr
                    $ dft (rank*chunkSize) chunkSize
                    $ (map (\x -> polar x 0) manifestedDArr :: DPull (Complex Float))
            resultV <- thawArr resultV

            -- On nodes that are not the root node, just send the
            -- result to the root node
            onAllBut root mpi_comm_world $ gatherSend resultV root mpi_comm_world

            -- On the root node, recieve the data
            -- from the other nodes and print
            -- the DFT of the original data!
            on root mpi_comm_world $ do
                        arr <- newArr arrSize :: Run (DArr (Complex Float))
                        gatherRecv arr resultV mpi_comm_world
                        for (0, 1, Excl arrSize)
                            (\i ->
                                do
                                  c <- getArr arr i
                                  printf "%f, %f\n" (realPart c) (imagPart c)
                            ) 

            -- finish and exit
            finish

-- | Communicators and groups
commGroup :: Run ()
commGroup = do
    (rank, size) <- setup
    let colour = rank `div` 2
    comm2 <- commSplit colour rank mpi_comm_world
    rank' <- getRank comm2
    size' <- getSize comm2
    printf "I'm %d, new rank and size: (%d, %d)\n" rank rank' size'
    finish
