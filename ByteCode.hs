module ByteCode
  ( Instruction(..)
  , ByteCode(..)
  , ByteCodes
  , Compare(..)
  , Location
  , sizeOf
  , sizeOfBC
  , parse ) where

import Data.Binary
import Data.List
import Data.Char
import Data.Bits
import Numeric 

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Instruction
  = AALoad
  | ALoad Word8
  | ALoadC Int
  | InvokeSpecial Word16
  | InvokeVirtual Word16
  | InvokeStatic Word16
  | ILoad Word8 | ILoadC Int 
  | LLoad Word8 | LLoadC Int 

  | IConstC Int 
  | LConstC Int

  | IStoreC Int 
  | LStoreC Int
  | AStoreC Int 
  
  | IStore Word8
  | LStore Word8
  | AStore Word8 

  | PutField Word16
  | GetStatic Word16
  | Return | IReturn | AReturn
  | LDC Word8
  | If_ACmp Compare Word16 
  | Goto Word16
  | New Word16
  | Dup
  | LCmp
  | If Compare Word16 
  | If_ICmp Compare Word16
  | AThrow
  | IAdd | LAdd | FAdd | DAdd
  | BiPush Word8
  | IInc Word8 Word8
 deriving (Eq,Show)

data Compare = Eq | Ne | Lt | Ge | Gt | Le
  deriving (Eq,Show)

type Location = Int

data ByteCode = BC Location Instruction Int
type ByteCodes = IntMap ByteCode

instance Show ByteCode where
    show (BC l i _) = "(0x" ++ (showHex l "") ++ "): " ++ show i

data ByteTable 
  = BTUnary Instruction
  | BTBinary (Word8 -> Instruction) 
  | BTTrinary (Word8 -> Word8 -> Instruction)

(~>) :: Word8 -> Instruction -> (Word8, ByteTable)
(~>) x y = (x, BTUnary y)
(~~>) :: Word8 -> (Word8 -> Instruction) -> (Word8, ByteTable)
(~~>) x y = (x, BTBinary y)
(~~~>) :: Word8 -> (Word8 -> Word8 -> Instruction) -> (Word8, ByteTable)
(~~~>) x y = (x, BTTrinary y)
(~~>>) :: Word8 -> (Word16 -> Instruction) -> (Word8, ByteTable)
(~~>>) x y = x ~~~> n'
  where n' = (y .) . (\i1 i2 -> ((fromIntegral i1) `shift` 8 .|. (fromIntegral i2)))
        {- n  i1 i2 = y $ case i1 `testBit` 7 of
            True  -> -(0xffff - n' i1 i2) -- - (n' (i1 `clearBit` 7) i2)
            False -> n' i1 i2 -}

parseBC :: [Word8] -> (Instruction, [Word8])
parseBC (x:xs) = case lookup x bt of
    Nothing -> error $ "no opcode for: " ++ show x
    Just (BTUnary ins) -> (ins, xs)
    Just (BTBinary f)  -> (f $ head xs, drop 1 xs)
    Just (BTTrinary f)  -> (f (head xs) (head . tail $ xs), drop 2 xs)
bt = 
    [ 
      0x02 ~> IConstC (-1)
    , 0x03 ~> IConstC 0
    , 0x04 ~> IConstC 1 
    , 0x05 ~> IConstC 2
    , 0x06 ~> IConstC 3
    , 0x07 ~> IConstC 4
    , 0x08 ~> IConstC 5
    , 0x09 ~> LConstC 0
    , 0x0a ~> LConstC 1
    , 0x10 ~~> BiPush
    , 0x12 ~~> LDC
    , 0x15 ~~> ILoad
    , 0x1a ~> ILoadC 0
    , 0x1b ~> ILoadC 1
    , 0x1c ~> ILoadC 2
    , 0x1d ~> ILoadC 3
    , 0x1e ~~> LLoad
    , 0x1f ~> LLoadC 0
    , 0x20 ~> LLoadC 1
    , 0x21 ~> LLoadC 2
    , 0x22 ~> LLoadC 3
    , 0x2a ~> ALoadC 0
    , 0x2b ~> ALoadC 1
    , 0x2c ~> ALoadC 2
    , 0x2d ~> ALoadC 3
    , 0x32 ~> AALoad
    , 0x36 ~~> IStore
    , 0x37 ~~> LStore
    , 0x3a ~~> AStore
    , 0x3b ~> IStoreC 0
    , 0x3c ~> IStoreC 1
    , 0x3d ~> IStoreC 2
    , 0x3e ~> IStoreC 3
    , 0x3f ~> LStoreC 0
    , 0x40 ~> LStoreC 1
    , 0x41 ~> LStoreC 2
    , 0x42 ~> LStoreC 3

    , 0x4b ~> AStoreC 0
    , 0x4c ~> AStoreC 1
    , 0x4d ~> AStoreC 2
    , 0x4e ~> AStoreC 3
    , 0x59 ~> Dup
    , 0x60 ~> IAdd
    , 0x61 ~> LAdd
    , 0x62 ~> FAdd
    , 0x63 ~> DAdd

    , 0x84 ~~~> IInc
    , 0x94 ~> LCmp

    , 0x99 ~~>> If Eq
    , 0x9a ~~>> If Ne
    , 0x9b ~~>> If Lt
    , 0x9c ~~>> If Ge
    , 0x9d ~~>> If Gt
    , 0x9e ~~>> If Le
    , 0x9f ~~>> If_ICmp Eq
    , 0xa0 ~~>> If_ICmp Ne
    , 0xa1 ~~>> If_ICmp Lt
    , 0xa2 ~~>> If_ICmp Ge
    , 0xa3 ~~>> If_ICmp Gt
    , 0xa4 ~~>> If_ICmp Le

    , 0x19 ~~> ALoad
    , 0xa5 ~~>> If_ACmp Eq
    , 0xa6 ~~>> If_ACmp Ne
    , 0xa7 ~~>> Goto
    , 0xac ~> IReturn
    , 0xb0 ~> AReturn
    , 0xb1 ~> Return
    , 0xb2 ~~>> GetStatic
    , 0xb5 ~~>> PutField 
    , 0xb6 ~~>> InvokeVirtual
    , 0xb7 ~~>> InvokeSpecial
    , 0xb8 ~~>> InvokeStatic
    , 0xbb ~~>> New
    , 0xbf ~> AThrow
    ]

example :: [Word8]
example =  [42,183,0,1,42,27,181,0,2,42,28,181,0,3,177]


parse :: [Word8] -> Location -> ByteCodes
parse a b = IM.fromList $ map (\x@(BC l _ _) -> (l,x)) (parse' a b)

parse' :: [Word8] -> Int -> [ByteCode]
parse' [] _      = []
parse' xs offset = 
  let (inst, rest) = parseBC xs
      size :: Int
      size = fromIntegral $ sizeOf (head xs)
    in BC (fromIntegral offset) inst size : parse' rest (size + offset)

sizeOf :: Word8 -> Word8
sizeOf a = case lookup a bt of
    Nothing -> error $ "no opcode for: " ++ show a
    Just (BTUnary _) -> w8
    Just (BTBinary _) -> w16
    Just (BTTrinary _) -> w24
 where
    (w8,w16,w24,w32) = (1,2,3,4)

sizeOfBC :: ByteCode -> Int
sizeOfBC (BC _ _ s) = s

--fancy x = putStrLn . unlines . map show . parse x
