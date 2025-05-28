module EnvC where
import GHC.Stack(HasCallStack)
import {-# SOURCE #-} ValC(Val, Fcn)

type MappingV = [(Val, Val)]
mkFcn :: HasCallStack => MappingV -> Fcn
mkIntFcn :: HasCallStack => MappingV -> Fcn
