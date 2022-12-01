exists X .
exists Y .
exists Z .
Y = all{X = Z};
X = (false|true);
Z = false;
Y

<#
p = new-promise
when-bound X -> ...
when-all p -> ...
unify Y, t0
unify X, false
when-bound Z -> ...
unify Z, false
check false, false
resolve p
unify t0, (false)
backtrack:
  unify t0, (false)
  resolve p
  unify Z, false
  when-bound Z -> ...
  unify X, false
unify X, true
when-bound Z -> ...
unify Z, false
check true, false
reject p
uinfy t0, ()
#>
