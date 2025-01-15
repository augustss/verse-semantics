data B a = B
data E a = E

instance Monad B
instance Monad E

instance Functor B
instance Functor E

instance Applicative B
instance Applicative E

switch :: B a -> E (B a) -> B a
switch = undefined

switches :: B a -> B (E (B a)) -> B a
b `switches` be =
  do a <- b
     eb <- be
     mb <- look eb
     case mb of
       Nothing -> b `switches` be
       Just b' -> b' `switche

look :: E a -> B (Maybe a)
look = undefined


