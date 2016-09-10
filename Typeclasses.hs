{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ScopedTypeVariables,
             GADTs #-}
module Typeclasses where
import qualified Prelude as P
import Feldspar.Representation
import Feldspar.Data.Vector
import Feldspar.Run

class DManifestable a b where
    manifested :: a -> Run (Manifest (Data b))

instance (Syntax (Data a)) => DManifestable [Data a] a where
    manifested xs = listManifest xs

instance (Syntax (Data a)) => DManifestable (Data a) a where
    manifested a = manifested [a]

class MPIReferable a where
    refer :: a -> Run (FunArg Data PrimType')

instance (PrimType' a, Type a) => MPIReferable (Data a) where
    refer a = fmap refArg $ initRef a

instance (PrimType' (Internal a), Syntax a) => MPIReferable (IArr a) where
    refer a = return $ iarrArg a

instance (PrimType' (Internal a), Syntax a) => MPIReferable (Arr a) where
    refer a = return $ arrArg a

instance (MPIReferable a, Slicable a) => MPIReferable (Nest a) where
    refer a = refer (unnest a)

class MPITypeable a where
    mpiType :: a -> String

instance MPITypeable (Data Int8) where
    mpiType _ = "MPI_BYTE"

instance MPITypeable (Data Int16) where
    mpiType _ = "MPI_SHORT"

instance MPITypeable (Data Int32) where
    mpiType _ = "MPI_INT"

instance MPITypeable (Data Int64) where
    mpiType _ = "MPI_LONG"

instance MPITypeable (Data Word8) where
    mpiType _ = "MPI_UNSIGNED_CHAR"

instance MPITypeable (Data Word16) where
    mpiType _ = "MPI_UNSIGNED_SHORT"

instance MPITypeable (Data Word32) where
    mpiType _ = "MPI_UNSIGNED"

instance MPITypeable (Data Word64) where
    mpiType _ = "MPI_UNSIGNED_LONG"

instance MPITypeable (Data Float) where
    mpiType _ = "MPI_FLOAT"

instance MPITypeable (Data Double) where
    mpiType _ = "MPI_DOUBLE"

instance (MPITypeable (Data a), PrimType a, PrimType (Complex a)) => MPITypeable (Data (Complex a)) where
    mpiType x = mpiType (realPart x)

instance (MPITypeable a) => MPITypeable (IArr a) where
    mpiType _ = mpiType (undefined :: a)

instance (MPITypeable a) => MPITypeable (Arr a) where
    mpiType _ = mpiType (undefined :: a)

instance (MPITypeable a) => MPITypeable (Nest a) where
    mpiType _ = mpiType (undefined :: a)

class Sizeable a where
    size :: a -> Data Length

instance (Sizeable (Data a), Type (Complex a), PrimType' a, Type a, PrimType' (Complex a)) => Sizeable (Data (Complex a)) where
    size x = 2 * (size (realPart x))

instance Sizeable (Data Int8) where
    size _ = 1

instance Sizeable (Data Int16) where
    size _ = 1

instance Sizeable (Data Int32) where
    size _ = 1

instance Sizeable (Data Int64) where
    size _ = 1

instance Sizeable (Data Word8) where
    size _ = 1

instance Sizeable (Data Word16) where
    size _ = 1

instance Sizeable (Data Word32) where
    size _ = 1

instance Sizeable (Data Word64) where
    size _ = 1

instance Sizeable (Data Float) where
    size _ = 1

instance Sizeable (Data Double) where
    size _ = 1

instance (Type a, Sizeable (Data a)) => Sizeable (DArr a) where
    size b = length b * (size (example :: (Data a)))

instance (Type a, Sizeable (Data a)) => Sizeable (DIArr a) where
    size b = length b * (size (example :: (Data a)))

instance (Sizeable a, Slicable a) => Sizeable (Nest a) where
    size b = size (unnest b)
