# f1(x:int) := x
f2(x:int):int := x+1 ;
f2[3]
# f3(x:int)<succeeds>:int := x
# f4(x:int)<decides>:int := x
# f5(x:int)(y:int):int := (x,y)
# f( g(x:int):int ) := g[0]

# This should be rejected
# f9(x:int):int := x
# y := f9[y]

# f6 should be rejected: the fn body only knows that x:int
# f6(x:int := 3) := x=3

# f7 is ok: fn body only knows that x:int
# f7(x:int := 3)<decides> := x=3

# f8 should be rejected
# f8(x:int)<decides>:type{x=3} := x+1

# x10 := 3              # No wrapping of rhs
# x11<succeeds> := 3    # Wrap rhs in <succeeds>

# Clients should see just some(int)
# x12:int := 88

# 'main' should verify despite three's<decides>,
# because 'three' is transparent
# three(x:int)<decides> := x = 3;
# main() := y<succeeds>:three := 3

# F(X:int, G():type{X = 1}) :=
#   (H()<succeeds> := X = 1 ;
#   H() ;
#   G() ) ;
# F(2, G() := G())

# f9( x := 3|4):int := x+1;
# y:int
# f9[y]

