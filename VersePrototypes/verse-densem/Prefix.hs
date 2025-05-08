module Main where

import Data.List( isPrefixOf )
import Test.QuickCheck
import Test.QuickCheck.Poly( A )

prop_Prefix_Append (OK xs) (OK ys) =
  isOK [ x++y | x <- xs, y <- ys ]

isOK :: Eq a => [[a]] -> Bool
isOK []     = True
isOK [x]    = True
isOK (x:xs) = all (x =!=) xs && isOK xs

(=!=) :: Eq a => [a] -> [a] -> Bool
x =!= y = not (x `isPrefixOf` y) && not (y `isPrefixOf` x)

newtype OK = OK [[A]] deriving ( Eq, Show )

instance Arbitrary OK where
  arbitrary = sized $ \n ->
    do xs <- arb n
       return (OK xs)
   where
    arb 0 =
      do return []
    
    arb n =
      do xs <- arb (n-1)
         mx <- arbitrary `suchThatMaybe` ((`all` xs) . (=!=))
         case mx of
           Nothing -> return xs
           Just x  -> return (x:xs)
  
  shrink (OK xs) =
    [ OK xs' | xs' <- shrink xs, isOK xs' ]

main = quickCheck prop_Prefix_Append

