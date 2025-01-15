
data S = [Name] :- E
data E = Val V | Fail | V :=: E | E :>: E | S :|: S | One S
type V = VAL Name
type H = VAL ()

data VAL a = Int Int | Tup [V] | VAR a

data RHS = Rigid{ the :: H } | Flexi{ the :: H }
type Env = [(Name,RHS)]

scope :: S -> Env -> (Result,Env)
scope (xs :- e) env =
  let (c, env') = expr e (env <| [(x,Flexi (VAR()))|x<-xs])
   in (c, [(y,h)|(y,h)<-env',y `notElem` xs])

data Result
  = VAL V
  | WRONG
  | FAIL
  | (Result,Env) :||: (Result,Env)

expr :: E -> Env -> (Result,Env)
expr (Val v) env =
  (VAL (env v), env)

expr (v :=: e) env =
  -- something something

expr (e1 :>: e2) env =
  mu $ \env' ->
    let (r1,env1) = expr e1 (env <| env')
        (r2,env2) = expr e2 env1
     in (r1 |> r2, env2)
 where
  mu :: (Env -> (a,Env)) -> (a,Env)
  mu = error "just compute the least fixpoint"

  VAL _ |> r = r
  FAIL  |> _ = FAIL
  WRONG |> _ = WRONG

  ((rL,envL) :||: (rR,envR)) |> _ =
    (mu $ \env' ->
       let (r2,env2) = expr e2 (envL <| env')
        in (rL |> r2, env2))
    :||:
    (mu $ \env' ->
       let (r2,env2) = expr e2 (envR <| env')
        in (rL |> r2, env2))

expr (e1 :|: e2) env =
  (expr e1 env :||: expr e2 env, env)

expr (One s) env =
  let (r,_) = scope s [(x,Rigid (the h))|(x,h)<-env]
   in (one [r], env)
 where
  one (VAL v : _)
    | isHNF v         = VAL v
    | otherwise       = WRONG
  one (FAIL : rs)     = one rs
  one (WRONG : rs)    = WRONG
  one ((r1:||:r2):rs) = one (r1:r2:rs)
  one []              = FAIL

-- fixpoints in one instead of ;

expr (e1 :>: e2) env =
  let (r1,env1) = expr e1 env
      (r2,env2) = expr e2 env1
   in (r1 |> r2, env2)
 where
  mu :: (Env -> (a,Env)) -> (a,Env)
  mu = error "just compute the least fixpoint"

  VAL _ |> r = r
  FAIL  |> _ = FAIL
  WRONG |> _ = WRONG

  ((rL,envL) :||: (rR,envR)) |> _ =
    


expr (One s) env =
  mu $ \env' ->
    let (r,env2) = scope s [(x,Rigid (the h))|(x,h)<-(env <| env')]
     in (one [r], env2)


