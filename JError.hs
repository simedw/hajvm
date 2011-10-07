module JError
  (JError(..)
  ) where

import Control.Monad.Error

data JError 
  = ErrorMsg String
  | ErrorLoadClass 
 deriving Show

instance Error JError where
  strMsg x = ErrorMsg x
