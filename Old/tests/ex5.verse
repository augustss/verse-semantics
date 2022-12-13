{
sumProd(xs:array(int)):tuple(int,int) := sumProdLoop(0, xs, (0, 1));
sumProdLoop(i:int, xs:array(int), sp := (sum:int, prod:int)):tuple(int,int) :=
  if (x:=xs[i]) then sumProdLoop(i+1, xs, (x+sum, x*prod)) else sp;
(s, p) := sumProd(1,2,3,4,5);
print(s+p);
}
