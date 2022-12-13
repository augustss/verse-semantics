{
x:int;
typedef{1|2};
x where x>0;
case(x+1) of { 0 => 1; 1 => 1; n => n-1 };
#
x&&y;
x||y;
#
x:int := e;
## not currently allowed (x,y) := e;
f(x:int) := e;
x+y;
x<y
}
