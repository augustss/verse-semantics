PRELUDE := ();  # HACK: Marker that we have loaded this
any(x:any) := x;
int(x:any) := { isInt$[x]; x };
float(x:any) := { isFlt$[x]; x };
string(x:any) := { isStr$[x]; x };
nat(x:any) := { isInt$[x]; x >= 0; x };
operator'->'(s:any,t:any)(f:any)(x:any) := { isFcn$[f] || isArr$[f]; t[f[s[x]]] };  # (bi-)functor
prefix'[]'(t:any)(as:any) := { isArr$[as]; for(a:as) in t[a] };  # functor
# is this the option type?
prefix'?'(t:any)(x:any) := { isArr$[x]; length[x] <= 1; for(a:x) in { t[a] } };  # functor
prefix'^'(t:any)(p:any) := { isPtr$[p]; t[deref$[p]]; p };  # not a functor
#tuple(t1:any)(array{x1:any}) := array{t1[x1]};
tuple(t1:any, t2:any)(x1:any, x2:any) := (t1[x1], t2[x2]);
#tuple(t1:any, t2:any, t3:any)(x1:any, x2:any, x3:any) := (t1[x1], t2[x2], t3[x3]);
# tuple ...

postfix'^'(p:any) := { isPtr$[p]; deref$[p] };
new(t:any)(x:any) := { new$[t[x]] };
assign(p:any,x:any) := { isPtr$[p]; assign$[p,x]; x };
false := array{};
operator'>' (x:int, y:int) := { intGT$[x, y]; x };
operator'>='(x:int, y:int) := { intGE$[x, y]; x };
operator'<' (x:int, y:int) := { intLT$[x, y]; x };
operator'<='(x:int, y:int) := { intLE$[x, y]; x };
truth(x:any) := array{x};

operator'..'(i:int, n:int) := { if (i > n) then :false else (i | ((i+1)..n)) };


