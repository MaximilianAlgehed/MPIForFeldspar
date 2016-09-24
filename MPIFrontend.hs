{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             ScopedTypeVariables,
             GADTs,
             TypeOperators,
             ConstraintKinds #-}
module MPIFrontend where
import qualified Language.Syntactic.Syntax as S
import Language.Embedded.Imperative
import Language.Syntactic.Decoration
import MPIForFeldspar
import Feldspar.Representation
import Typeclasses
import qualified Prelude as P
import Feldspar.Data.Vector
import Feldspar.Run
import Feldspar.Run.Representation

type MPICMD = CommCMD :+: RunCMD

data CommCMD fs a where
    On :: (pred a, DManifestable a Word32)
       => exp a
       -> exp Communicator
       -> prog ()
       -> CommCMD (Param3 prog exp pred) ()

data GroupConstructs sig where
    Union        :: GroupConstructs (Group S.:-> (Group S.:-> S.Full Group))
    Intersection :: GroupConstructs (Group S.:-> (Group S.:-> S.Full Group))

type MPIDomain = (FeldConstructs S.:+: GroupConstructs) :&: TypeRepFun

newtype MPIData a = MPIData {unMPIData :: S.ASTF MPIDomain a}

newtype MPIRun a =
    MPIRun {unRun :: ProgramT
                        MPICMD
                        (Param2 MPIData PrimType')
                        (Program CompCMD (Param2 MPIData PrimType'))
                        a
           }
