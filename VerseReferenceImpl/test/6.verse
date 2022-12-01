exists X .
exists Y .
all{X = true} = ();
Y = false|true;
X = false|true;
(X, Y)

<#
p = new-promise
when-bound X -> ...
when-all p -> ...
unify t0, ()
unify Y, false
unify X, false
check false, true
reject p
check t0, ()
backtrack:
  reject p
  unify X, false
unify X, true
resolve p
check (true), ()
backtrack:
  resolve p
  unify X, true
  unify Y, false
unify Y, true
...
#>
