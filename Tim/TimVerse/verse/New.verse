# Unsure what is wrong?
#test(X00):
#  f(x:int,y:int):= if(int[x]){x}else{y}
#  a:int=1
#  b:int=if(int[a]){1}else{2}
#  f[a,b]
#  a=1

# Feature?  Seems like converges is lying
test(S00):
  f()<converges><fails>:= {Err();1=2}
  #  f()<converges><fails>:int= {Err();1=2}  # opaque fun is rejected

# Bug? 
test(D00):
  f(x:int, g():type{x=3}):=
    x=3
    if (x=0 or x=1 or x=3). g()

test(S00):
  f(x:int,g():type{x=3}):=
    if (x=0 or x=1 or x=3). g()
    x=3

# Feature.
test(N04):
   f(x:int):= 1
   x:int
   f(x)

# Bug? This should not stall
test(R00):
  f()<interacts> :void=  Print(1)
  f()

# Bug! Print ok, and then nothing
test(S00):
  if (y=2). Print("ok")
  Print("Where am I?")
  y:=2

# Weird? Surprisingly succeeds
test(S00):
  f(c:char):=1 
  as:any => as=array{_} =>  x:=as[0] => bs:array{:int}=> bs=as => f[x] 

# Surprising in combination with the next example
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

# Loop does not hide failure
test(F00):
  0 => {Err("Hello"),2=3}
  0 => {2=3,Err("Hello")}

# Bug!  The first fails to verify. The second stalls the verifier.
#test(S00):
#  f(x:int):int= if(x=0) {x} else {f(0)}
#  f(x:int):= if(x=0){x} else {f(0)}

# Bug! Runtime stalled
test(R00):
  # f(x:int):= x  # does not verify
  # f(x:int):int= 1  # verifies and runs fine. Should it?
  f(x:int):int= x 
  a&b:int
  b=f(a)
  a=f(b)

# Bug! This can only run if we execute f() ahead of g(). Effect inversion!
test(S00):
  f(x:int)<writes>:int= x
  g(x:int)<reads>:void= 1
  b:int
  g(b)
  b=f(1)

# Ok? Unsure. variable a is unused, does it need to be resolved?
test(A00):
  a&b:int
  b=1

# Bug! We don't print 2 (Already seen. I wanted to check ordering)
test(S00):
  a&b:int
  if(int[a]). Print(1)
  if(int[b]). Print(2)
  b=1
  a=1

# Feature? Choice in types, is that allowed?
test(X00):
   f(x:type{3|3})<succeeds>:int := 55;
   f[3]

# looping does not prevent use of the assumption
test(S00):
  f(x:int,g():type{x=0}):=
     x=0
     Err()
     g()

