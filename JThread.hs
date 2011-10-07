module JThread
  ( JThread
  , run
  ) where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Error hiding (throwError)
import qualified Control.Monad.Trans.Error as E
import Control.Concurrent.MVar

import Util
import JError
import ClassFile (ClassName)
import JGlobal hiding (run)
import qualified JGlobal as Global
import JFrame hiding (run)
import qualified JFrame  as Frame


type JThread = StateT JThreadState (ErrorT JError IO)

data JThreadState = JThreadState 
  { globalState   :: MVar JGlobalState
  , frames        :: [JFrameState]
  , currentClass  :: String -- more to come
  }

newThread gState = JThreadState
  { globalState = gState
  , frames      = []
  , currentClass = ""
  }


run :: JThread a -> JThreadState -> IO (Either JError (a,JThreadState))
run = (runErrorT .) . runStateT

pushFrame :: JFrameState -> JThread ()
pushFrame f = modify (\ts -> ts {frames = f : frames ts})

popFrame :: JThread ()
popFrame = modify (\ts -> ts {frames = tail (frames ts)})

execute :: ClassName -> String -> JThread ()
execute cn mn = do
    fromG $ linking cn 
    pushFrame newFrame
    return ()

-- util --
fromG :: JGlobal a -> JThread a
fromG p = do 
    gStateM <- gets globalState
    gState  <- liftIO $ takeMVar gStateM
    (res,state) <- errorM . liftIO $ Global.run p gState
    liftIO $ putMVar gStateM state
    return res

test = do 
    g <- newMVar newGlobal
    res <- run (execute "example/Main.class" "main") (newThread g)
    case res of
        Left err -> putStrLn . show $ err
        Right v  -> putStrLn "done"

throwError :: JError -> JThread a
throwError = lift . E.throwError
