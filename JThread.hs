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
import Data.IntMap  (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory

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
  , heap          :: IntMap JVariable
  }

newThread gState = JThreadState
  { globalState  = gState
  , frames       = []
  , currentClass = ""
  , heap         = IM.empty
  }


run :: JThread a -> JThreadState -> IO (Either JError (a,JThreadState))
run = (runErrorT .) . runStateT

pushFrame :: JFrameState -> JThread ()
pushFrame f = modify (\ts -> ts {frames = f : frames ts})

popFrame :: JThread ()
popFrame = modify (\ts -> ts {frames = tail (frames ts)})

execute :: ClassName -> String -> JThread ()
execute cn mn = do
    inG $ initialize cn 
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
       VString str <- vLookup 1 -- first is this..
       liftIO $ putStrLn str
       popFrame
    x -> throwError . strMsg $ "Native method not implemented" ++ show x

interpreter :: ClassName -> Location -> ByteCodes -> JThread ()
interpreter cn pc code = do
  opcode <- maybeM (strMsg "bytecode out of bounds") $ IM.lookup pc code
  case unBC opcode of
    BC.ALoadC  i -> next $ do
        vLookup i >>= pushOS
    BC.AStoreC i -> next $ popOS >>= vInsert i
    BC.IConstC i -> next $ pushOS (VInteger i) 
    BC.IStoreC i -> next $ popOS >>= vInsert i
    BC.ILoadC  i -> next $ vLookup i >>= pushOS
    BC.LDC     i -> next $ getStringCP cn i >>= pushOS 
    BC.GetStatic i     -> next $ getFieldRef cn i >>= 
                                 pushOS . VStaticField cn . refName
    BC.InvokeVirtual i -> next $ getMethodRef cn i >>= call
    BC.InvokeStatic  i -> next $ getMethodRef cn i >>= callStatic
    BC.InvokeSpecial i -> next $ getMethodRef cn i >>= call
    BC.IAdd            -> next $ math (+)
    BC.ISub            -> next $ math (-)
    BC.IMul            -> next $ math (*)
    BC.IInc i c        -> next $ vLookup i >>= \v -> vInsert i (v + fromIntegral c)
    BC.PutField i  -> next $ do
        ref <- getFieldRef cn i
        v   <- popOS
        objref  <- popOS
        heapUpdate objref (refName ref) v
    BC.GetField i -> next $ do
        classFile <- inG (getClassFile cn)
        classInfo <- inG (getClassInfo cn)
        ref <- getFieldRef cn i
        objref  <- popOS
        obj     <- heapLookupRef objref
        pushOS $ getField obj (refName ref)
    BC.Dup -> next $ peekOS >>= pushOS
    BC.New i   -> next $ do 
        className <- getClassCP cn i
        obj       <- new className 
        heapInsert obj >>= pushOS . VObjectRef
        
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

icmp :: JVariable -> JVariable -> Compare -> Bool
icmp v1 v2 Eq = v1 == v2
icmp v1 v2 Ne = v1 /= v2
icmp v1 v2 Lt = v1 < v2
icmp v1 v2 Le = v1 <= v2
icmp v1 v2 Gt = v1 > v2
icmp v1 v2 Ge = v1 >= v2


new :: ClassName -> JThread JVariable
new cn = do
    inG $ initialize cn
    return $ VObject M.empty -- TODO: set default values

heapInsert :: JVariable -> JThread Int
heapInsert v = do
    h <- gets heap
    let next = 1 + IM.size h
    modify (\ts -> ts { heap = IM.insert next v h} )
    return next


heapLookupRef :: JVariable -> JThread JVariable
heapLookupRef (VObjectRef i) = heapLookup i

heapLookup :: Int -> JThread JVariable
heapLookup i = do 
    h <- gets heap
    maybeM (strMsg "heapLookup") $ IM.lookup i h

heapUpdate :: JVariable -> String -> JVariable -> JThread ()
heapUpdate (VObjectRef ref) = heapUpdate' ref 

heapUpdate' :: Int -> String -> JVariable -> JThread ()
heapUpdate' i field value = do
    oldObj <- heapLookup i
    let newObj = updateField oldObj field value
    modify (\ts -> ts { heap = IM.insert i newObj (heap ts) })

updateField :: JVariable -> String -> JVariable -> JVariable
updateField (VObject fields) field v = VObject $ M.insert field v fields

getField :: JVariable -> String -> JVariable
getField (VObject fields) field = fromJust $ M.lookup field fields

-- | calls a reference method
-- | return has to pop the frame 
call :: Reference -> JThread ()
call ref = do
    let argumentLength = 1 + getNumberOfArgs (refType ref)
    args     <- replicateM argumentLength popOS
    pushFrame newFrame
    mapM (uncurry vInsert) $ zip [0..] (reverse args)
    inG $ linking (className ref)
    callMethod (className ref) (refName ref)
callStatic :: Reference -> JThread ()
callStatic ref = do
    let argumentLength = getNumberOfArgs (refType ref)
    args     <- replicateM argumentLength popOS
    pushFrame newFrame
    mapM (uncurry vInsert) $ zip [0..] (reverse args)
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

getFieldRef :: ClassName -> Word16 -> JThread Reference
getFieldRef cn i = do
    classInfo <- inG $ getClassInfo cn
    maybeM (strMsg "getFieldRef") $ CI.getFieldRef classInfo (fromIntegral i)

getMethodRef :: ClassName -> Word16 -> JThread Reference
getMethodRef cn i = do
    classInfo <- inG $ getClassInfo cn
    maybeM (strMsg "getMethodRef") $ CI.getMethodRef classInfo (fromIntegral i)



-- get string for the constant pool
getStringCP :: ClassName -> Word8 -> JThread JVariable
getStringCP cn i = do
    classFile <- inG $ getClassFile cn
    return . VString $ CF.getStringCP classFile (fromIntegral i)

getClassCP :: ClassName -> Word16 -> JThread ClassName
getClassCP cn i = do
    classFile <- inG (getClassFile cn)
    return $ CF.getClassCP classFile (fromIntegral i)

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
    liftIO $ setCurrentDirectory "example"
    res <- run (execute "Main.class" "main") (newThread g)
    case res of
        Left err -> putStrLn . show $ err
        Right v  -> putStrLn "done"

throwError :: JError -> JThread a
throwError = lift . E.throwError
