cnf(refl, axiom, leq(X,X)).

cnf(more, axiom, leq(n3,X) | leq(X,n2)).

fof(goal1, negated_conjecture,
  ~leq(n3,input)
).

fof(goal2, negated_conjecture,
    ( leq(n3,input)
    | ~leq(input,n2)
    )
).


