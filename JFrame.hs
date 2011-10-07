module JFrame
  ( JFrame
  , JFrameState
  , newFrame
  , Index
  , run
  , pushOS 
  , popOS
  , peekOS
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Error hiding (throwError)
import qualified Control.Monad.Trans.Error as E

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import JError
import JVariable

type Index = Int

type JFrame = StateT JFrameState (ErrorT JError Identity)

data JFrameState  = JFrameState
    { localVariables :: IntMap JVariable
    , operandStack   :: [JVariable]
    }

-- | runs the JFrame monad
run :: JFrame a -> JFrameState -> Either JError (a,JFrameState)
run = ((runIdentity .) runErrorT .) . runStateT

newFrame = JFrameState 
  { localVariables = IM.empty
  , operandStack   = []
  }

{---- operandStack operations ----}

-- | add an variable to the top of the operandStack
pushOS :: JVariable -> JFrame ()
pushOS v = modify (\fs -> fs { operandStack = v : operandStack fs })

-- | removes and returns the top of the operandStack
popOS :: JFrame JVariable
popOS = gets (head . operandStack) >>= \v -> 
        modify (\fs -> fs {operandStack = tail (operandStack fs)}) >>
        return v

-- | peeks at the top element
peekOS :: JFrame JVariable
peekOS = popOS >>= \v -> pushOS v >> return v

{---- localVariables operations ----}

-- | insert an variable at an index 
insert :: JVariable -> Index -> JFrame ()
insert v i = modify (\fs -> fs {localVariables = IM.insert i v (localVariables fs)})

-- | lookups an variable at an index
lookup :: Index -> JFrame JVariable
lookup i = gets (IM.lookup i . localVariables) >>= \x -> case x of
    Nothing -> throwError . strMsg $ "lookup: index " ++ show i
    Just  r -> return r

-- util --
throwError :: JError -> JFrame a
throwError = lift . E.throwError
