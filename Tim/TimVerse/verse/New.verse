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