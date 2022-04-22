incr(x:int):int := x+1;
ap(f(:int):int, x:int) := f(x);
ap := fn(f(:int):int, x:int){f(x)};
ap(f(:int)<succeeds>:int, x:int)<succeeds> := f(x);

ap := fn(f(:int):int, x:int)<succeeds>{f(x)};

g(x:int):int := do { y:=x+1; y };
g := fn(x:int){ int(do { y:=x+1; y }) };

