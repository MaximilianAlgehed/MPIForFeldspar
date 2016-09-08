{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
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

class MPIReferable a where
    refer :: a -> Run (FunArg Data PrimType')

instance (PrimType' a, Type a, Syntax a) => MPIReferable (Data a) where
    refer a = fmap refArg $ initRef a

instance (PrimType' (Internal a), Syntax a) => MPIReferable (Manifest a) where
    refer a = return $ iarrArg a

class MPITypeable a where
    mpiType :: a -> String

instance MPITypeable (Data Int32) where
    mpiType _ = "MPI_INT"

instance (MPITypeable a) => MPITypeable (Manifest a) where
    mpiType _ = mpiType (undefined :: a)

instance Finite (Data Int32) where
    length _ = 1
