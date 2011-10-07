module ClassInfo 
  ( ClassInfo(..)
  , prepare
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Word
import Data.Bits
import Data.Maybe

import ClassFile
import qualified ClassFile as CF
import ByteCode
import qualified ByteCode as BC

data Reference = Reference
  { className :: ClassName
  , refName   :: String
  , refType   :: String
  }
  deriving Show

data ClassInfo = ClassInfo 
    { super     :: Maybe ClassName
    , methods   :: [MethodDefition]
    , mrefs     :: IntMap Reference
    , fieldrefs :: IntMap Reference
    }
  deriving Show

data MethodDefition = MethodDefition
    { method_name :: String
    , method_code :: ByteCodes
    , access_flag :: AccessFlag
    , isStatic    :: Bool
    , isNative    :: Bool
    }
  deriving Show

data AccessFlag = Public | Private | Protected
  deriving Show


prepare :: ClassName -> ClassFile -> ClassInfo
prepare cn cf = ClassInfo (getSuperClass cf)
                          (prepareMethods cf (CF.methods cf))
                          (prepareMethodRefs cn cf $ zip [1..] $ CF.cp_info cf)
                          (prepareFieldRefs cf     $ zip [1..] $ CF.cp_info cf)
  where
    getSuperClass cf = case super_class cf of
        0 -> Nothing
        n -> Just (getName cf n ++ ".class")

prepareMethods :: ClassFile -> [Method_Info] -> [MethodDefition]
prepareMethods cf [] = []
prepareMethods cf (x:xs) = MethodDefition
    { method_name = name 
    , method_code = code
    , access_flag = access_flag
    , isStatic    = static
    , isNative    = native
    } : prepareMethods cf xs
  where 
    name   = ustring (cp_info cf !!! m_name_index x)
    static = m_access_flags x `testBit` static_flag
    native = m_access_flags x `testBit` native_flag
    access_flag = case (m_access_flags x `testBit` public_flag 
                       ,m_access_flags x `testBit` private_flag
                       ,m_access_flags x `testBit` protected_flag) of
        (True,_,_) -> Public
        (_,True,_) -> Private
        (_,_,True) -> Protected
        _       -> Public     -- if nothing is specified it's probably is Public  
    code = head $ catMaybes (map codes (m_attributes_info x) ++ [Just IM.empty])
    codes :: Attribute_Info -> Maybe ByteCodes
    codes x = case x of
        CAI _ _ _ c _ _ _  _ -> Just $ BC.parse c 0
        _   -> Nothing


prepareFieldRefs :: ClassFile -> [(Int,ConstantPool_Info)] -> IntMap Reference
prepareFieldRefs _  []       = IM.empty
prepareFieldRefs cf ((n,x):xs) = case x of
    C_Fieldref_Info cindex nt -> IM.insert n 
        (mkReference (getName cf cindex ++ ".class" ) cf nt)
        (prepareFieldRefs cf xs)
    _ -> prepareFieldRefs cf xs 

prepareMethodRefs :: ClassName -> ClassFile -> [(Int,ConstantPool_Info)] -> IntMap Reference
prepareMethodRefs _ _  []          = IM.empty
prepareMethodRefs ci cf ((n,x):xs) = case x of
    C_Methodref_Info cindex nt -> IM.insert n 
        (mkReference (fixName cindex) cf nt)
        (prepareMethodRefs ci cf xs)
    _ -> prepareMethodRefs ci cf xs
 where 
    name cindex = getName cf cindex ++ ".class"
    -- self refence doesn't give the full className, why? I don't know :/
    fixName cindex = case (name cindex) == (reverse . takeWhile (/= '/') . reverse $ ci) of
        True  -> ci
        False -> name cindex 

mkReference :: ClassName -> ClassFile -> Word16 -> Reference
mkReference cn cf cw = case cp_info cf !!! cw of
    C_NameAndType_Info na ty -> Reference 
      { className = cn
      , refName   = getName cf na
      , refType   = getName cf ty
      }
    _ -> error "mkReference"

-- recursivly find names and stuff
getName :: ClassFile -> Word16 -> String
getName cf w = case cp_info cf !!! w of
    C_Class_Info n -> getName cf n
    x -> ustring x


--- this should probably be in classfile...
public_flag    = 0 -- 0x1
private_flag   = 1 -- 0x2
protected_flag = 2 -- 0x4
static_flag    = 3 -- 0x8
native_flag    = 8 -- 0x100


