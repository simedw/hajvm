module ClassFile
  ( ClassFile(..)
  , ClassName
  , parse
  , Method_Info(..)
  , ConstantPool_Info(..)
  , Attribute_Info(..)
  , (!!!)
  , getStringCP
  ) where


import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8
import Data.Binary.Get
import Data.Word
import Control.Monad

import Control.Monad.Error

import JError

{- Specifications
   http://java.sun.com/docs/books/vmspec/2nd-edition/html/ClassFile.doc.html#20080
-}

type ClassName = String

(!!!) :: Num a => [b] -> a -> b
(!!!) a b = help a (b-1)
 where
 help [] n   = error $"!!! out of bounds (" ++ show n ++ ")"
 help (x:xs) 0 = x
 help (x:xs) n = xs `help` (n-1) 

getTimes :: Num b => b -> Get a -> Get [a]
getTimes 0 f = return []
getTimes n f = liftM2 (:) f (getTimes (n-1) f)

{-
    cp_info {
      u1 tag;
      u1 info[];
    }
 -}

data ConstantPool_Info 
  = C_Class_Info { name_index :: Word16 }
  | C_Fieldref_Info { class_index :: Word16, name_and_type_index :: Word16 }
  | C_Methodref_Info { class_index :: Word16, name_and_type_index :: Word16 }
  | C_InterfaceMethodref_Info {class_index :: Word16, name_and_type_index :: Word16}
  | C_String_Info { string_index :: Word16 }
  | C_Integer_Info { bytes :: Word32 }
  | C_Float_Info { bytes :: Word32 }
  | C_Long_Info { high_bytes :: Word32, low_bytes :: Word32 }
  | C_Double_Info { high_bytes :: Word32, low_bytes :: Word32 }
  | C_NameAndType_Info { name_index :: Word16, descriptor_index :: Word16 }
  | C_Utf8_Info { len :: Word16, ubytes :: [Word8], ustring :: String }
 deriving Show


parseCP = do 
  tag <- getWord8
  case tag of
    1 -> do -- utf8
      len <- getWord16be
      ubytes <- read len
      let ustring = toString $ B.pack ubytes
      return $ C_Utf8_Info { len = len, ubytes = ubytes, ustring = ustring}
    3 -> do -- integer
      bytes <- getWord32be
      return $ C_Integer_Info { bytes = bytes }
    4 -> do -- float
      bytes <- getWord32be
      return $ C_Float_Info { bytes = bytes }
    5 -> do -- long
      high_bytes <- getWord32be
      low_bytes <- getWord32be
      return $ C_Long_Info { high_bytes = high_bytes, low_bytes = low_bytes }
    6 -> do -- double
      high_bytes <- getWord32be
      low_bytes <- getWord32be
      return $ C_Double_Info { high_bytes = high_bytes, low_bytes = low_bytes }
    7 -> do -- class
      name_index <- getWord16be
      return $ C_Class_Info { name_index = name_index }
    8 -> do -- string
      string_index <- getWord16be
      return $ C_String_Info { string_index = string_index}
    9 -> do -- fieldref
      class_index <- getWord16be
      name_and_type_index <- getWord16be
      return C_Fieldref_Info { class_index = class_index
                             , name_and_type_index = name_and_type_index }
    10 -> do -- methodref
      class_index <- getWord16be
      name_and_type_index <- getWord16be
      return C_Methodref_Info { class_index = class_index
                              , name_and_type_index = name_and_type_index }
    11 -> do -- interfacemethodref
      class_index <- getWord16be
      name_and_type_index <- getWord16be
      return C_InterfaceMethodref_Info { class_index = class_index
                                       , name_and_type_index = name_and_type_index }
    12 -> do -- nameandtype
      name_index <- getWord16be
      descriptor_index <- getWord16be
      return $ C_NameAndType_Info { name_index = name_index
                                  , descriptor_index = descriptor_index }
    n -> error $ "Unkown tag: " ++ show n

 where
   read :: Word16 -> Get [Word8]
   read 0 = return []
   read n = liftM2 (:) getWord8 (read (n-1))

{-
    field_info {
        u2 access_flags;
        u2 name_index;
        u2 descriptor_index;
        u2 attributes_count;
        attribute_info attributes[attributes_count];
    }
 -}

data Field_Info = FI
    { f_access_flags :: Word16
    , f_name_index :: Word16
    , f_descriptor_index :: Word16
    , f_attribute_count :: Word16
    , f_attribute_info :: [Attribute_Info]
    }
 deriving Show

parseField :: [ConstantPool_Info] -> Get Field_Info
parseField cpi = do
  f_access_flags <- getWord16be
  f_name_index   <- getWord16be
  f_descriptor_index <- getWord16be
  f_attribute_count <- getWord16be
  f_attribute_info <- getTimes f_attribute_count (parseAttribute cpi)
  return $ FI
    { f_access_flags = f_access_flags
    , f_name_index = f_name_index
    , f_descriptor_index = f_descriptor_index
    , f_attribute_count = f_attribute_count
    , f_attribute_info = f_attribute_info
    }

{-
    method_info {
        u2 access_flags;
        u2 name_index;
        u2 descriptor_index;
        u2 attributes_count;
        attribute_info attributes[attributes_count];
    }
 -}


data Method_Info = MI
  { m_access_flags :: Word16
  , m_name_index :: Word16
  , m_descriptor_index :: Word16
  , m_attributes_count :: Word16 
  , m_attributes_info  :: [Attribute_Info]
  }
 deriving Show

parseMethod :: [ConstantPool_Info] -> Get Method_Info
parseMethod cpi = do
  m_access_flags <- getWord16be 
  m_name_index <- getWord16be
  m_descriptor_index <- getWord16be
  m_attributes_count <- getWord16be
  m_attributes_info <- getTimes m_attributes_count (parseAttribute cpi)
  return $ MI
    { m_access_flags = m_access_flags
    , m_name_index = m_name_index
    , m_descriptor_index = m_descriptor_index
    , m_attributes_count = m_attributes_count
    , m_attributes_info  = m_attributes_info
    }


{-
    attribute_info {
        u2 attribute_name_index;
        u4 attribute_length;
        u1 info[attribute_length];
    }
-}

data Attribute_Info =
   AI { attribute_name_index :: Word16
      , attribute_length  :: Word32
      , attribute_data  :: [Word8]
      }
 | CAI { max_stack :: Word16
       , max_locals :: Word16
       , code_length :: Word32
       , code :: [Word8]
       , exception_table_length :: Word16
       , exception_table :: [ExceptionTable]
       , c_attributes_count :: Word16
       , c_attributes_info :: [Attribute_Info]
       }
 deriving Show

data ExceptionTable = ET
    { start_pc :: Word16
    , end_pc   :: Word16
    , handler_pc  :: Word16
    , catch_type :: Word16
    }
  deriving Show

parseExceptionTable :: Get ExceptionTable
parseExceptionTable = do
    start_pc <- getWord16be
    end_pc <- getWord16be
    handler_pc <- getWord16be
    catch_type <- getWord16be
    return ET 
        { start_pc = start_pc
        , end_pc = end_pc
        , handler_pc = handler_pc
        , catch_type = catch_type
        }
parseAttribute :: [ConstantPool_Info] -> Get Attribute_Info
parseAttribute cpi = do
    attribute_name_index <- getWord16be
    attribute_length <- getWord32be
    case cpi !!! attribute_name_index of
        C_Utf8_Info _ _ a | a == "Code" -> do
            max_stack   <- getWord16be
            max_locals  <- getWord16be
            code_length <- getWord32be
            code        <- getTimes code_length getWord8
            exception_table_length <- getWord16be
            exception_table <- getTimes exception_table_length parseExceptionTable
            c_attributes_count <- getWord16be
            c_attributes_info  <- getTimes c_attributes_count (parseAttribute cpi)
            return $ CAI { max_stack = max_stack
                         , max_locals = max_locals
                         , code_length = code_length
                         , code = code
                         , exception_table_length = exception_table_length
                         , exception_table = exception_table
                         , c_attributes_info = c_attributes_info
                         , c_attributes_count = c_attributes_count
                         }
        _ -> do
            attribute_data <- getTimes attribute_length getWord8
            return $ AI
                { attribute_name_index = attribute_name_index
                , attribute_length = attribute_length   
                , attribute_data = attribute_data
                }

data ClassFile = CF
  { magic :: Word32
  , minor_version :: Word16
  , major_version :: Word16
  , constant_pool_count :: Word16
  , cp_info :: [ConstantPool_Info]
  , access_flags :: Word16
  , this_class :: Word16
  , super_class :: Word16
  , interfaces_count :: Word16
  , interfaces :: [Word16]
  , fields_count :: Word16
  , fields :: [Field_Info]
  , methods_count :: Word16
  , methods :: [Method_Info]
  , attributes_count :: Word16
  , attributes :: [Attribute_Info]
  }
 deriving Show

parseCF :: Get ClassFile
parseCF = do
  magic <- getWord32be
  minor_version <- getWord16be
  major_version <- getWord16be
  constant_pool_count <- getWord16be
  cp_info <- getTimes (constant_pool_count-1) parseCP
  access_flags <- getWord16be
  this_class <- getWord16be
  super_class <- getWord16be
  interfaces_count <- getWord16be
  interfaces <- getTimes interfaces_count getWord16be
  fields_count <- getWord16be
  fields <- getTimes fields_count (parseField cp_info)
  methods_count <- getWord16be
  methods <- getTimes methods_count (parseMethod cp_info)
  attributes_count <- getWord16be
  attributes <- getTimes attributes_count (parseAttribute cp_info)
  return $ CF
    { magic = magic
    , minor_version = minor_version
    , major_version = major_version
    , constant_pool_count = constant_pool_count
    , cp_info = cp_info
    , access_flags = access_flags
    , this_class = this_class
    , super_class = super_class
    , interfaces_count = interfaces_count
    , interfaces = interfaces
    , fields_count = fields_count
    , fields = fields
    , methods_count = methods_count
    , methods = methods
    , attributes_count = attributes_count
    , attributes = attributes
    }

parse :: FilePath -> IO (Either JError ClassFile)
parse classFile = flip catch (\_ -> return $ throwError ErrorLoadClass) $ do
    input <- B.readFile classFile
    return . Right $ runGet parseCF input


getStringCP :: ClassFile -> Int -> String
getStringCP cf i = case (cp_info cf) !!! i of
        C_String_Info i -> getString cf i 
        

getString :: ClassFile -> Word16 -> String
getString cf i = case cp_info cf !!! i of
    C_Class_Info n -> getString cf n
    x              -> ustring x
