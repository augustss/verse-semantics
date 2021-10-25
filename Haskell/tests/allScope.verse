{
5;
do {x:any; x};
do {x:any; y:any; x = y};
do {x:any; f:any; f[x]};
do {x:any; f:any; f(x)};
do {x=>x};
do {x:any; x | {y:any; y}};
do {x:any; (x,x,2,y:any,y) };
do {if(x:=1) then (let(y:=2) in x+y) else 3};
do {for(x:=1|2) (x+1)}
}
