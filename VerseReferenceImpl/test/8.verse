exists X .
all {
  X = false;
  false|true
};
X = true

<#
p = new-promise
when-bound X -> ...
when-all p -> ...
unify X, true
check true, false
reject p
backtrack:
  when-all p -> ...
when-all p -> ...
unify t0, ()
#>
