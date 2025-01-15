module Core where

type Name
  = String

data Block
  = Block Heap Constr
 deriving ( Eq, Ord, Show )

data Constr
  = Name := Val
  | Name :=@ (Name, Name)
  | Skip
  | Fail
  | Constr :>: Constr
  | Block :|: Block
  | First Block Block
 deriving ( Eq, Ord, Show )

data Val
  = Var Name
  | HNF HNF
 deriving ( Eq, Ord, Show )

data HNF
  = Int Integer
  | Arr [Name]
  | Lam Lambda
 deriving ( Eq, Ord, Show )

type Lambda
  = () -- not decided what this is yet

