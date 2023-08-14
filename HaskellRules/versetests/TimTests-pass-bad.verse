# pass, bad
test(F00){1=2 => 3=4}
test(F00){92=93 => 94=95}
test(F00){92=93 => 94=95,91}
test(F00){as:=(2,3=4) => as=(7,_)}
test(F00){as:=(2,3=4) => 2=5}
test(F00){(a:=2,b:=3=4) => a=7}
test(F00){x:=let(false[]){3} => x=4}
test(F00){x:=7=9 => 6=8}

# test(F00){nat[2/3]}   #TODO:rationals
# test(F00){int[2/3]}   #TODO:rationals