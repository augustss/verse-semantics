module Exp where
import Data.List
import Data.Data

--------------------
---- Abstract syntax

type Ident = String

data Exp
  = Var Ident | Int Integer | Prim Op | App Exp Exp | Equ Exp Exp
  | Seq Exp Exp | Def Ident Exp | Colon Exp | Fail | Tup [Exp]
  | If Exp Exp Exp | Fun OC Exp Exp
  | Choice Exp Exp | All Exp | For Exp Exp
  | Where Exp Exp
  deriving (Eq, Ord, Data)

data Op = Oint | Ogt | Oadd
  deriving (Eq, Ord, Show, Data)

data OC = Open | Closed
  deriving (Eq, Ord, Show, Data)

instance Show Exp where
  showsPrec _ (Var s) = showString s
  showsPrec p (Int i) = showsPrec p i
  showsPrec _ (Prim o) = showString (drop 1 $ show o)
  showsPrec _ (App e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (Equ e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (Seq e1 e2) = showParen (p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
  showsPrec p (Where e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec p (Def x e) = showParen (p > 5) $ showString x . showString " := " . showsPrec 6 e
  showsPrec _ (Colon e) = showString ":" . showsPrec 10 e
  showsPrec _ Fail = showString "fail"
  showsPrec _ (Tup es) = showString "<" . showString (intercalate "," $ map show es) . showString ">"
  showsPrec _ (If e1 e2 e3) = showString "if " . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (showsPrec 0 e3)
  showsPrec p (Choice e1 e2) = showParen (p > 4) $ showsPrec 5 e1 . showString " | " . showsPrec 5 e2
  showsPrec _ (All e) = showString "all" . showBraces (showsPrec 0 e)
  showsPrec _ (For e1 e2) = showString "for" . showParen True (showsPrec 0 e1) . showBraces (showsPrec 0 e2)
  showsPrec _ (Fun q e1 e2) = showString (if q == Open then "fun_o" else "fun_c") .
                              showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2)

showBraces :: (String -> String) -> (String -> String)
showBraces a = showString "{" . a . showString "}"

--------------------
---- Find all identifiers defined by := in this scope

dI :: Exp -> [Ident]
dI = checkDup . sort . dI'
  where
    checkDup (x:x':xs) | x == x' = error $ "Duplicate definition of " ++ x
                       | otherwise = x : checkDup (x':xs)
    checkDup xs = xs

dI' :: Exp -> [Ident]
dI' (App e1 e2) = dI' e1 ++ dI' e2
dI' (Equ e1 e2) = dI' e1 ++ dI' e2
dI' (Seq e1 e2) = dI' e1 ++ dI' e2
dI' (Where e1 e2) = dI' e1 ++ dI' e2
dI' (Tup es) = concat (map dI' es)
dI' (Def i e) = i : dI' e
dI' (Colon e) = dI' e
dI' _ = []

