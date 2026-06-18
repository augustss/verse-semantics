incr(x:int):int := x+1;
ap1(f(:int):int, x:int) := f(x);
ap2 := fn(f(:int):int, x:int){f(x)};
ap3(f(:int)<succeeds>:int, x:int)<succeeds> := f(x);

ap4 := fn(f(:int):int, x:int)<succeeds>{f(x)};

g1(x:int):int := do { y:=x+1; y };
g2 := fn(x:int){ int(do { y:=x+1; y }) };
