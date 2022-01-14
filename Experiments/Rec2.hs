{-# LANGUAGE RecursiveDo #-}

module Rec2 where

import Debug.Trace

-- (newWay [1,2,3]) is a lot cheaper than (mdoWay [1,2,3])
newWay vs = xys
   where
      xys = aux shape xys

      shape :: [ (Int,Int) -> (Int,Int) ]
      shape = [ \p -> (xf p,yf p)
              | v <- vs   -- does not depend
              , xf <- if expensive v then [ \(x,y) -> y, \(x,y) -> y ] else [ \(x,y) -> y ]
              , yf <- [ \_ -> 1, \_ -> 2 ]
              ]

mdoWay vs = mdo { v <- vs; x <- if expensive v then [y,y] else [y];  y <- [1,2]; return (x,y)  }

expensive :: Int -> Bool
expensive v = trace ("expensive " ++ show v) (even v)

aux :: [p -> q] -> [p] -> [q]
aux []     _         = []
aux (f:fs) ~(xy:xys) = f xy : aux fs xys
