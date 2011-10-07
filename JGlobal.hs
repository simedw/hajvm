module JGlobal
  ( JGlobal
  , JGlobalState
  , newGlobal
  , run
  , load
  , linking
  , initialize
  ) where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Error hiding (throwError)
import qualified Control.Monad.Trans.Error as E
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Util
import JError
import ClassFile
import qualified ClassFile as CF 
import ClassInfo hiding (prepare)
import qualified ClassInfo as CI
import ClassData

type JGlobal = StateT JGlobalState (ErrorT JError IO)

data JGlobalState = JGlobalState
    { loadedClasses :: M.Map ClassName ClassFile
    , linkedclasses :: M.Map ClassName ClassInfo
    , initializedClasses :: M.Map ClassName ClassData
    }

newGlobal = JGlobalState 
  { loadedClasses = M.empty
  , linkedclasses = M.empty
  , initializedClasses = M.empty
  }

-- | runs the JGlobal monad
run :: JGlobal a -> JGlobalState -> IO (Either JError (a,JGlobalState))
run = (runErrorT .) . runStateT

-- | returns a classfile
getClassFile :: ClassName -> JGlobal ClassFile
getClassFile cn = gets (M.lookup cn . loadedClasses) >>= \x -> case x of
    Nothing -> throwError . strMsg $ "getClassFile: " ++ cn
    Just  v -> return v

getClassInfo :: ClassName -> JGlobal ClassInfo
getClassInfo cn = gets (M.lookup cn . linkedclasses) >>= \x -> case x of
    Nothing -> throwError . strMsg $ "getInfoFile: " ++ cn
    Just  v -> return v



-- | loads an classfile if not already loaded
load :: ClassName -> JGlobal ()
load cn = unlessM (isLoaded cn) $ do
    liftIO . putStrLn $ "loading: " ++ cn
    classFile <- errorM (liftIO . CF.parse $ cn)
    modify (\gs -> gs { loadedClasses = M.insert cn classFile (loadedClasses gs)})
    return ()

isLoaded :: ClassName -> JGlobal Bool
isLoaded cn = liftM (maybe False (const True)) $ gets (M.lookup cn . loadedClasses) 


linking :: ClassName -> JGlobal ()
linking cn = unlessM (isLinked cn) $ do
    -- first verification
    -- then preparetion
    -- and lastly resolving, but since we use lazy resolve we only need to do 
    -- preparetion now
    liftIO . putStrLn $ "linking: " ++ cn
    classInfo <- prepare cn
    modify (\gs -> gs { linkedclasses = M.insert cn classInfo (linkedclasses gs)})

-- | prepares a class if not already prepared
-- | will load the class if needed
prepare :: ClassName -> JGlobal ClassInfo
prepare cn = do
    unlessM (isLoaded cn) (load cn)
    classFile <- getClassFile cn
    prepare' cn classFile

prepare' :: ClassName -> ClassFile -> JGlobal ClassInfo
prepare' cn cf = return $ CI.prepare cn cf

isLinked :: ClassName -> JGlobal Bool
isLinked cn = liftM (maybe False (const True)) $ 
              gets (M.lookup cn . linkedclasses) 

-- | initialize a class
-- | will load and/or prepare if needed
initialize :: ClassName -> JGlobal ()
initialize cn = do
    unlessM (isLoaded cn) (load cn)
    unlessM (isLinked cn) (linking cn)
    classInfo <- getClassInfo cn
    let superClass = super classInfo
    unless (isNothing $ superClass) $ initialize (fromJust $ superClass)
    classData <- initialize' cn
    modify (\gs -> gs { initializedClasses = 
                            M.insert cn classData (initializedClasses gs)})
    return ()

initialize' :: ClassName -> JGlobal ClassData
initialize' _ = return $ ClassData 

isInitialized :: ClassName -> JGlobal Bool
isInitialized cn = liftM (maybe False (const True)) $ 
                   gets (M.lookup cn . initializedClasses) 


-- util --
throwError :: JError -> JGlobal a
throwError = lift . E.throwError

