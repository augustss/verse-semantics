## TODO


- [] elim-def-overlap <----- HEREHEREHEREHEREHEREHEREHERE
- [] uni-uni see below
  - [] unify+rspc+semicolon (used in eq-join?)


CHECK: In UNI vs. UNI

  x = \lambda; x = x ; e

Can JOIN deref-h and u-scalar as we never subst lambda into x=x

 ## Can we modify

[WF-Eq] to add preconditions

    v != x    (delete it first)

    v scalar ==> x not in fv(e1)

if v IS scalar, you could have used deref-s
to replace all free occ of x in e1 first ...
