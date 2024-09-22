# Bug! These are two distinct x variables, but the verifier complains
test(N04):
   f(x:int):= 1
   x:int
   f(x)

# Bug? This should not stall, should it?
test(R00):
  f()<interacts> :void=  Print(1)
  f()

# Bug! Print ok, and then nothing
test(S00):
  if (y=2). Print("ok")
  Print("Where am I?")
  y:=2

# Unsure? I was trying to get an array variable
test(C00):
  f(y:type(array{2})) := 1

# Weird? Surprisingly succeeds
test(S00):
  f(c:char):=1 
  as:any => as=array{_} =>  x:=as[0] => bs:array{:int}=> bs=as => f[x] 

# Surprising in combination with the next example  (RI: ok)
test(S00):
  ty(x:int)<succeeds>:int= if(x=0) then x else 0
  g(x:ty)<succeeds>:int=1 

# The above complained about ty being abstract, but here it does not... 
test(S00):
  ty(x:int)<succeeds>:int= x
  g(x:ty)<succeeds>:int=1  

# Feature? + not supported on abstract values?
test(A00):
  x:int=1; x+1