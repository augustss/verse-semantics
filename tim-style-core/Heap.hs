module Heap where

import Data.List
import Data.Maybe
import Core

type Entry
  = ([Name], Maybe HNF)

data Heap
  = Heap [Entry]
  | Failed
 deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

heap :: [([Name], Maybe HNF)] -> Heap
heap = Heap . sort

--------------------------------------------------------------------------------

(~) :: Val -> Val -> Heap
Var x ~ Var y | x == y             = Heap []
              | otherwise          = Heap [(sort [x,y],Nothing)]
Var x ~ HNF v | x `notElem` free v = Heap [([x],Just v)]
HNF v ~ Var x                      = Var x ~ HNF v

HNF (Int a)  ~ HNF (Int b)  | a == b                 = Heap []
HNF (Arr vs) ~ HNF (Arr ws) | length vs == length ws = inter [ v ~ w | (v,w) <- zip vs ws ]

_ ~ _ = Failed

inter :: [Heap] -> Heap
inter []                     = Heap []
inter [h]                    = h
inter (Failed  : _)          = Failed
inter (_       : Failed : _) = Failed
inter (Heap [] : hs)         = inter hs

inter (Heap ((xs,mv):es1) : Heap es2 : hs) =
  inter ( zipWith (~) ws (tail ws)
       ++ Heap es1
        : Heap (insert (ys,mw) esNo)
        : hs
        )
 where
  (esYes, esNo) = partition (any (`elem` xs)) es2
  ys = unionList (xs:[ zs | (zs,_) <- esYes ])
  ws = [ v | Just v <- [mv] ] ++ [ w | (_,Just w) <- esYes ]
  mw = headMaybe ws

(/\) :: Heap -> Heap -> Heap
h1 /\ h2 = inter [h1,h2]

