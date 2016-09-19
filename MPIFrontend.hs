{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ScopedTypeVariables #-}
module MPIForFeldspar where
import MPIForFeldspar
import Feldspar.Representation
import Typeclasses
import qualified Prelude as P
import Feldspar.Data.Vector
import Feldspar.Run

data MPICMD = RunCMD :+: GroupCMD :+: CommCMD
