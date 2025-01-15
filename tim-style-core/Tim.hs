module Tim where

type Name
  = String

type Lambda
  = (Name, Block, Name, Block) -- or whatever

data Block
  = Block Heap Constr
  | Failed
 deriving ( Eq, Ord, Show )

data Constr
  = Name := Val
  | Name :=: Name
  | Name :=@ (Name, Name)
  | Skip
  | Fail
  | Constr :>: Constr
  | Block :|: Block
  | First Block Block
 deriving ( Eq, Ord, Show )

data Val
  = Int Integer
  | Arr [Name]
  | Lam Lambda
 deriving ( Eq, Ord, Show )

type Heap
  = [([Name], Maybe Val)]

look :: Heap -> Name -> Maybe ([Name], Maybe Val, Heap)
look [] x       = Nothing
look ((xs,mv):h) x
  | x `elem` xs = Just (xs, mv, h)
  | otherwise   = do (ys, mw, h') <- look h x; return (ys, mw, (xs,mv):h')

(/\) :: Heap -> Heap -> Maybe Heap
[]                 /\ h2 = h2
(([],_)       :h1) /\ h2 = h1 /\ h2
(([_],Nothing):h1) /\ h2 = h1 /\ h2
(([x],Just v) :h1) /\ h2 = (h1 /\)             `fmap` unify h2 x v
((x:y:xs,mv)  :h1) /\ h2 = (((y:xs,mv):h1) /\) `fmap` unifyName h2 x y
 
unifyName :: Heap -> Name -> Name -> Maybe Heap
unifyName h x y =
  case look h x of
    Nothing -> Just $
      case look h y of
        Nothing           -> ([x,y],Nothing):h
        Just (ys, mv, h') -> (x:ys,mv):h'

    Just (xs, mv, h')
      | y `elem` xs -> Just h
      | otherwise   ->
        case look y h' of
          Just (ys, mw, h'') ->
            case (mv, mw) of
              (Nothing, _)     -> (xs++ys, mw):h''
              (_, Nothing)     -> (xs++ys, mv):h''
              (Just v, Just w) -> unifyVal v w ((xs++ys, mv):h'')

          Nothing -> Just ((y:xs,mv):h')

unifyVal :: Heap -> Val -> Val -> Maybe Heap
unifyVal h (Int k1) (Int k2) | k1 == k2 =
  Just h

unifyVal h (Arr xs) (Arr ys) | length xs == length ys =
  foldr (\(x,y) h -> unifyName h x y) h (xs `zip` ys)

unifyVal h _ _ = Nothing

unify :: Heap -> Name -> Val -> Maybe Heap
unify h x v =
  case look h x of
    Just (xs, mv, h') ->
      case mv of
        Nothing -> Just ((xs,Just v):h')
        Just v' -> unifyVal h v v'

    Nothing -> Just (([x],Just v):h)

----

step :: Block -> Maybe Block
step Failed =
  Nothing

step (Block h Skip) =
  Nothing

step (Block _ Fail) =
  Just Failed

step (Block h (x :=: y)) =
  Just $ case unifyName h x y of
           Nothing -> Failed
           Just h' -> Block h' Skip

step (Block h (x := y)) =
  Just $ case unify h x v of
           Nothing -> Failed
           Just h' -> Block h' Skip

step (Block h (c1 :>: c2)) =
  case step (Block h c1) of
    Nothing ->
      case step (Block h c2) of
        Nothing ->
          Nothing
        
        Just Failed ->
          Just Failed
      
        Just (Block h' c2') ->
          Just (Block h' (c1 .>. c2'))
    
    Just Failed ->
      Just Failed
  
    Just (Block h' c1') ->
      Just (Block h' (c1' .>. c2))
 where
  Skip .>. c2   = c2
  c1   .>. Skip = c1
  c1   .>. c2   = c1 :>: c2

step (Block h (Failed :|: b2)) =
  case b2 of
    Failed      -> Just Failed
    Block h2 c2 ->
      case h /\ h2 of
        Nothing -> Just Failed
        Just h' -> Just (Block h' c2)

step (Block h (Block h1 c1 :|: b2)) =
  case h /\ h1 of
    Nothing ->
      Just (Block h (Failed :|: b2))

    Just h' ->
      case step (Block h' c1) of
        Just Failed ->
          Just (Block h (Failed :|: b2))
        
        Just (Block h'' c1') ->
          Just (Block h (Block h'' c1' :|: b2))
          
        Nothing ->
          case b2 of
            Failed ->
              Just (Block h' c1)
            
            Block h2 c2 ->
              case h /\ h2 of
                Nothing ->
                  Just (Block h' c1)

                Just h2' ->
                  case step (Block h2' c2) of
                    Just Failed ->
                      Just (Block h' c1)
                    
                    Just (Block h'' c2') ->
                      Just (Block h (Block h1 c1 :|: Block h'' c2'))
                      
                    Nothing ->
                      Nothing

step (Block h (First Failed _ _)) =
  Just Failed
  
step (Block h (First (Block h1 c1) b2)) =
  case h /\ h1 of
    Nothing ->
      Just Failed
    
    Just h' ->
      case c1 of
        Skip ->
          case b2 of
            Failed ->
              Just Failed
            
            Block h2 c2 ->
              case h' /\ h2 of
                Nothing ->
                  Just Failed
                
                Just h'' ->
                  Block h'' c2

        _ ->
          case step (Block h c1) of
            Just Failed ->
              Just Failed
            
            Just (Block h'' c1') ->
              Just (Block h (First (Block h'' c1') b2))
            
            Nothing ->
              Nothing
              
