module JVariable where

data JVariable 
  = VInteger Int
  | VStaticField String String
  | VString String
