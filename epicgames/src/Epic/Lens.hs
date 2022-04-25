module Epic.Lens
  ( module Control.Lens,
    update,
  )
where

import Control.Lens hiding (argument, op, parts, rewriteM, universe, (<.>))

-- Like @(%~)@, i.e., update a field specified by a lens by apply a function to it,
-- but with the function being monadic.
update :: (Monad m) => ALens' s a -> (a -> m a) -> s -> m s
update l f s = do
  a <- f (s ^# l)
  pure $ s & l #~ a
