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

# Feature? Printing parameter does not work, instead ".."
test(S00):
  x:int
  f(a:int) := a
  Print(f(x))
  x=2

# Bug! Should not print ".." 
test(S00):
  x:int
  f(a:int) := x
  Print(f(1))
  x=2

# OK. Just checking what is allowed
test(S00):
  f1()<writes><reads><interacts><decides>:= 1
  f2()<fails> := 1=2
  # f2()<fails> := 1  # not allowed

# Bug! The last one should be rejected
test(S00):
   x:int => f():type{x=0} =>  {f(); x=0}
   x:int => f():type{x=0} =>  {if(int[x]){f()}else{}; x=0}
   x:int => f():type{x=0} =>  {if(x=1 or x=0){f()}else{}; x=0}  # Not if x=1
test(D00):  # This one seems right even error is obscure
   x:int => f():type{x=0} =>  {if(x=1){f()}else{}; x=0}

# OK.  loop does not hide failure
test(F00):
  0 => {Err("Hello"),2=3}
  0 => {2=3,Err("Hello")}

# Bug!  The first fails to verify. The second stalls the verifier.
#test(S00):
#  f(x:int):int= if( x = 0 ) { x } else { f(0) }
#  f(x:int):= if( x = 0 ) { x } else { f(0) }


