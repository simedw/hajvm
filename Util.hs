module Util where
import Control.Monad.Error.Class
import Control.Monad


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM f p = f >>= \v -> unless v p

errorM :: MonadError e m => m (Either e b) -> m b
errorM f = f >>= \x -> case x of
    Left  err -> throwError err
    Right v   -> return v 

