{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ScopedTypeVariables,
             GADTs,
             TypeOperators #-}
module MPIFrontend where
import Language.Syntactic.Syntax hiding ((:+:))
import Language.Embedded.Imperative
import Language.Syntactic.Decoration
import MPIForFeldspar
import Feldspar.Representation
import Typeclasses
import qualified Prelude as P
import Feldspar.Data.Vector
import Feldspar.Run
import Feldspar.Run.Representation

type MPICMD = RunCMD :+: CommCMD

data CommCMD fs a where
    On :: (DManifestable a Word32) => exp a -> exp Communicator -> prog () -> CommCMD (Param3 prog exp pred) ()

data GroupConstructs sig where
    Union        :: GroupConstructs (Group :-> (Group :-> Full Group))
    Intersection :: GroupConstructs (Group :-> (Group :-> Full Group))

type MPIDomain = (FeldConstructs :+: GroupConstructs) :&: TypeRepFun

newtype MPIData a = MPIData {unMPIData :: ASTF MPIDomain a}

newtype MPIRun a =
    MPIRun {unRun :: ProgramT
                        MPICMD
                        (Param2 MPIData PrimType')
                        (Program CompCMD (Param2 MPIData PrimType'))
           }
