module JVM where


import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Error
import Control.Monad.Trans.State

import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


type JGlobal = StateT JGlobalState (ErrorT ErrorMsg IO)
type JFrame  = StateT JFrameState  (ErrorT ErrorMsg Identity)
type JThread = StateT JThreadState (ErrorT ErrorMsg IO)

data JGlobalState = JGlobalState
    { loadedClasses :: M.Map String ClassFile
    , linkedclasses :: M.Map String ClassInfo
    }
data JFrameState  = JFrameState
    { localVariables :: IntMap Variable
    , operandStack   :: [Variable]
    }
data JThreadState = JThreadState 
    { gobalState   :: MVar JGlobalState
    , frames       :: [JFrameState]
    , currentClass :: String -- more to come
    }

data ErrorMsg = ErrorMsg

data Variable = Variable
data ClassFile
data ClassInfo





