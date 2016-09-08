{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Typeclasses where
import qualified Prelude as P
import Feldspar.Data.Vector
import Feldspar.Run

class DManifestable a b where
    manifested :: a -> Run (Manifest (Data b))

instance (Type a) => DManifestable [Data a] a where
    manifested xs = listManifest xs

instance (Type a) => DManifestable (Data a) a where
    manifested a = manifested [a]

class MPITypeable a where
    mpiType :: a -> String

instance MPITypeable (Data Int32) where
    mpiType _ = "MPI_INT"

instance Finite (Data Int32) where
    length _ = 1
