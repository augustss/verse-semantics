exists X .
exists Y .
Y = one{(X = true)|(X = false)};
X = false|true;
Y

<#
p0 = new-promise
when-bound X -> ...
when-all p0 -> ...
unify X, false
check X, true
reject p0
backtrack:
  when-all p0 -> ...
  when-bound X -> ...
check X, false
backtrack:
  unify X, false
  reject p0
  backtrack:
    when-all p0 -> ...
    when-bound X -> ...
unify X, true
check X, true
resolve p0
backtrack:
  when-all p0 -> ...
  when-bound X -> ...
check X, false
#>
