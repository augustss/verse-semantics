module Densem where

type INT = Int

data W
  = Con INT
  | Tup [W]
  | Fun (W -> WStar)

type Ident = String

type Env = Ident -> W

(x,v) +: rho = \y -> if y == x then v else rho y

-- expressions

data Val
  = Int INT
  | Var Ident
  | Op Op
  | Arr [Val]
  | Lam Ident Expr
 deriving ( Eq, Ord, Show )

data Op
  = GRT
  | ADD
  | INT
 deriving ( Eq, Ord, Show )

data Expr
  = Val Val
  | Fail
  | Expr :|: Expr
  | Expr :=: Expr
  | Val :@: Val
  | Exi Ident Expr
  | One Expr
  | All Expr
 deriving ( Eq, Ord, Show )

expr :: Expr -> Env -> WStar
expr (Val v)     rho = unit (val v rho)
expr Fail        rho = empty
expr (e1 :|: e2) rho = expr e1 rho `union` expr e2 rho
expr (e1 :=: e2) rho = expr e1 rho `inter` expr e2 rho
expr (v1 :@: v2) rho = val v1 rho `apply` val v2 rho
expr (Exi x e)   rho = bigUnion (\w -> expr e ((x,w) +: rho))
expr (One e)     rho = one (expr e rho)
expr (All e)     rho = unit (Tup (alL (expr e rho)))

val :: Val -> Env -> W
val (Var x)   rho = rho x
val (Int k)   rho = Con k
val (Op op)   rho = oper op
val (Lam x e) rho = Fun (\w -> expr e ((x,w) +: rho))
val (Arr vs)  rho = Tup [ val v rho | v <- vs ]

oper :: Op -> W
oper ADD = Fun (\w -> case w of
                        Tup [Con k1, Con k2] -> unit (Con (k1+k2))
                        _                    -> WRONG)
oper GRT = Fun (\w -> case w of
                        Tup [Con k1, Con k2] | k1 > k2 -> unit (Con k1)
                        _                              -> empty)
                        
oper INT = Fun (\w -> case w of
                        Con k -> unit (Con k)
                        _     -> empty)

apply :: W -> W -> WStar
apply (Con k)  _ = WRONG
apply (Tup ws) x = case x of -- DIFFERENT FROM DOCUMENT!
                     Con k | 0 <= k && k < length ws -> unit (ws !! k)
                     _                               -> empty
apply (Fun f)  w = f w

{-
-- set semantics

data WStar
  = BOTTOM
  | WRONG
  | Set (Set W)

type Set a = [a]

empty :: WStar
empty = undefined

unit :: W -> WStar
unit = undefined

union :: WStar -> WStar -> WStar
union = undefined

inter :: WStar -> WStar -> WStar
inter = undefined

bigUnion :: (W -> WStar) -> WStar
bigUnion = undefined

-- unimplementable

one :: WStar -> WStar
one = error "one"

alL :: WStar -> [W]
alL = error "all"
-}

-- set semantics

data WStar
  = BOTTOM
  | WRONG
  | Set (Set LW)

type LW = ([L],W)

data L = L | R

type Set a = [a]

empty :: WStar
empty = Set []

unit :: W -> WStar
unit w = Set [([],w)]

union :: WStar -> WStar -> WStar
BOTTOM `union` _      = BOTTOM
_      `union` BOTTOM = BOTTOM
WRONG  `union` _      = WRONG
_      `union` WRONG  = WRONG
Set s1 `union` Set s2 = Set $ [ (L : l, w) | (l,w) <- s1 ]
                           ++ [ (R : l, w) | (l,w) <- s2 ]

inter :: WStar -> WStar -> WStar
BOTTOM `inter` _      = BOTTOM
_      `inter` BOTTOM = BOTTOM
WRONG  `inter` _      = WRONG
_      `inter` WRONG  = WRONG
Set s1 `inter` Set s2 = Set $ [ (l1++l2, w) | (l1,w1) <- s1, (l2,w2) <- s2, w1 == w2 ]

bigUnion :: (W -> WStar) -> WStar
bigUnion = undefined

-- unimplementable

one :: WStar -> WStar
one = error "one"

alL :: WStar -> [W]
alL = error "all"

--------------------------------------------------------------------------------
-- NOTES FOR THE DOCUMENT
{-
* semantics of ; in the set semantics should be cross-product

* The use of k as a pattern for an integer constant is not always consistent.

* apply needs to be defined when the index is not a constant

* v is not always a value, sometimes it's a W

* 2.4: the semantics is not a sequence yet

* BOTTOM and WRONG propagation, does this make sense? I.e. one {2 | BOTTOM} should still produce the 2?

* what happens when there are an infinite number of choices?

* how to deal with WRONG correctly so that not everything gets infected with WRONG, e.g. exists x . x+x=5 will be wrong when x is a function.
-}


