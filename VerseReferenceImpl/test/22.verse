exists X .
all{(X = true)|(X = true)};
X = false

<#
p = new-promise
when-bound X -> ...
when-all p -> ...
unify X, false
check false, true
backtrack:
  when-all p -> ...
  when-bound X -> ...
fail
X
#>
