{
do { x };
do { 6 };
do { x:=0; };
do { x:int; x=(y:int) };
do { (f:t)[e:u] };
do { (f:t)(e:u) };
do { x => (y := 0) };
do { (x:=1) | (y:=2) };
do { (x:=1),2,(y:=3) };
do { if (x:=1) then (y:=x) else (z:=2) };
do { for(x:int) (y:=x) };
do { let(x:=1) in (y:=x); z:=y };
do { x:=1; 2; y:=3 }
}
