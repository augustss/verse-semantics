# f1(x:int) := x
# f2(x:int):int := x
# f3(x:int)<succeeds>:int := x
# f4(x:int)<decides>:int := x
# f5(x:int)(y:int):int := (x,y)
# f( g(x:int):int ) := g[0]

f2(x:int):int := x
y := f2[y]


# exists y.
#   y = (x := check<succ>{int[y]}; int[x])
# -->
# exists x y.
#   x = check<succ>{int[y]}
#   y = int[x]
#
#
# f2(x:int):int := x
# -->
# f2 := fun(x := :int){ x |> int }
# -->
# f2 := verify{...}
#       \i. j := x := M+[:int]P(i); B-[x |> int]E j
# -->
# f2 := verify{...}
#       \i. j := x := check<succ>{int[i]}; M-[x |> int]E
