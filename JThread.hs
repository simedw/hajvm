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
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Word

import ByteCode
import qualified ByteCode as BC
import ClassInfo hiding (getFieldRef, getMethodRef)
import qualified ClassInfo as CI
import JVariable
import Util
import JError
import ClassFile (ClassName)
import qualified ClassFile as CF
import JGlobal hiding (run)
import qualified JGlobal as Global
import JFrame hiding (run,popOS,pushOS,peekOS,insert,lookup)
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
    inG $ linking cn 
    pushFrame newFrame
    callMethod cn mn 
    return ()



callMethod :: ClassName -> String -> JThread ()
callMethod cn mn = do
    classInfo <- inG $ getClassInfo cn
    method <- maybeM (ErrorMsg "no such method") 
                     (getMethod classInfo mn)
    case isNative method of
        True  -> nativeCall cn mn method 
        False -> interpreter cn 0 (method_code method)

nativeCall :: ClassName -> String -> MethodDefition -> JThread ()
nativeCall cn mn md = case (cn,mn) of
    ("java/io/PrintStream.class","println") -> do
       VString str <- vLookup 0
       liftIO $ putStrLn str
       popFrame
    x -> throwError . strMsg $ "Native method not implemented" ++ show x

interpreter :: ClassName -> Location -> ByteCodes -> JThread ()
interpreter cn pc code = do
  opcode <- maybeM (strMsg "bytecode out of bounds") $ IM.lookup pc code
  case unBC opcode of
    BC.IConstC i -> next $ pushOS (VInteger i) 
    BC.IStoreC i -> next $ popOS >>= vInsert i
    BC.GetStatic i     -> next $ getFieldRef cn i >>= pushOS
    BC.LDC i           -> next $ getStringCP cn i >>= pushOS 
    BC.ILoadC i        -> next $ vLookup i >>= pushOS
    BC.InvokeVirtual i -> next $ getMethodRef cn i >>= call
    BC.InvokeStatic  i -> next $ getMethodRef cn i >>= call  
    BC.ISub            -> next $ math (-)
    BC.IMul            -> next $ math (*)
    BC.IInc i c        -> next $ vLookup i >>= \(VInteger v) -> 
                                                vInsert i (VInteger (v+1))
    BC.IReturn -> popOS >>= \res -> popFrame >> pushOS res
    BC.Return  -> popFrame
    BC.Goto offset -> interpreter cn (pc + offset) code
    BC.If_ICmp c loc -> do
      v2 <- popOS
      v1 <- popOS
      if icmp v1 v2 c
       then interpreter cn (pc + fromIntegral loc) code  
       else next $ return () -- just continue
    x -> throwError . strMsg $ "no such opcode: " ++ show x
  where
    next :: JThread a -> JThread ()
    next p = p >> interpreter cn (pc + BC.sizeOfBC (fromJust $ IM.lookup pc code)) code
    unBC (BC.BC _ x _ ) = x   
    math op = popOS >>= \v2 -> popOS >>= \v1 -> pushOS (v1 `op` v2)
icmp v1 v2 Eq = v1 == v2
icmp v1 v2 Ne = v1 /= v2
icmp v1 v2 Lt = v1 < v2
icmp v1 v2 Le = v1 <= v2
icmp v1 v2 Gt = v1 > v2
icmp v1 v2 Ge = v1 >= v2


-- | calls a reference method
-- | return has to pop the frame 
call :: Reference -> JThread ()
call ref = do
    let argumentLength = getNumberOfArgs (refType ref)
    args     <- replicateM argumentLength popOS
    pushFrame newFrame
    mapM (uncurry vInsert) $ zip [0..] args
    inG $ linking (className ref)
    callMethod (className ref) (refName ref)

-- | this is probably wrong, but every ; is a new argument
getNumberOfArgs :: String -> Int
getNumberOfArgs [] = 0
getNumberOfArgs ('(':xs) = getNumberOfArgs xs
getNumberOfArgs (')':xs) = 0
getNumberOfArgs ('I':xs) = 1 + getNumberOfArgs xs
getNumberOfArgs ('L':xs) = 1 + (getNumberOfArgs . tail . dropWhile (/= ';') $ xs)
getNumberOfArgs xs = error xs
--length . filter (==';') 

getFieldRef :: ClassName -> Word16 -> JThread JVariable
getFieldRef cn i = do
    classInfo <- inG $ getClassInfo cn
    ref <- maybeM (strMsg "getFieldRef") $ CI.getFieldRef classInfo (fromIntegral i)
    return $ VStaticField (className ref) (refName ref)

getMethodRef :: ClassName -> Word16 -> JThread Reference
getMethodRef cn i = do
    classInfo <- inG $ getClassInfo cn
    maybeM (strMsg "getMethodRef") $ CI.getMethodRef classInfo (fromIntegral i)



-- get string for the constant pool
getStringCP :: ClassName -> Word8 -> JThread JVariable
getStringCP cn i = do
    classFile <- inG $ getClassFile cn
    return . VString $ CF.getStringCP classFile (fromIntegral i)

pushOS :: JVariable -> JThread ()
pushOS = inF . Frame.pushOS 
popOS :: JThread JVariable
popOS = inF Frame.popOS
peekOS :: JThread JVariable
peekOS = inF Frame.peekOS

vInsert :: Index -> JVariable -> JThread ()
vInsert = (inF .) . flip Frame.insert 
vLookup :: Index -> JThread JVariable
vLookup = inF . Frame.lookup


-- util --
-- | execute inside a global
inG :: JGlobal a -> JThread a
inG p = do 
    gStateM <- gets globalState
    gState  <- liftIO $ takeMVar gStateM
    (res,state) <- errorM . liftIO $ Global.run p gState
    liftIO $ putMVar gStateM state
    return res
-- | execute inside a jframe
inF :: JFrame a -> JThread a
inF p = do
    frame <- gets (head . frames)
    (res,state) <- errorM . return $ Frame.run p frame
    modify (\ts -> ts {frames = state : tail (frames ts)})
    return res

test = do 
    g <- newMVar newGlobal
    res <- run (execute "example/Main.class" "main") (newThread g)
    case res of
        Left err -> putStrLn . show $ err
        Right v  -> putStrLn "done"

throwError :: JError -> JThread a
throwError = lift . E.throwError
