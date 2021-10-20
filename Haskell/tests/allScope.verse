{
5;
do {def{x}; x};
do {def{x}; def{y}; x = y};
do {def{x}; def{f}; f[x]};
do {def{x}; def{f}; f(x)};
do {x=>x};
do {def{x}; x | {def{y}; y}};
do {def{x}; (x,x,2,def{y},y) };
do {if(x:=1) then (let(y:=2) in x+y) else 3};
do {for(x:=1|2) (x+1)}
}
