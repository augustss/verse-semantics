# Tests commented out with #LA don't parse.
# Tests with a Q prefix on the tag are things that are not implemented yet.

#f(g():any):=g[]; f[h():=6]

# Abstraction propagation.
test(S00){f():=for(1) do :int => 0}						# NOTE:FOR
test(S00){f():=for(1|2) do :int => 0}					# NOTE:FOR
test(S00){f():=if(1=1) then :int else :nat => 0}		# NOTE:???
test(S00){f():=if(1=2) then :int else :nat => 0}		# NOTE:???
test(S00){f():=let() do :int => 0}						# NOTE:???
test(S00){f():type{(:nat)|(:int)} => 0}					# NOTE:???

# IsReady bug introduced by op::atom change.
#test(F00){for{i where array{2,4,6}[i:int]=4}=array{2}}

# Bad error: Is P00, should be F00, been here a long time.
#f(x:int):=x => f(3)=5

# Bringing verifier back up with NEWREGS.
test(S00){1}
test(S00){1=1}
test(F00){1=2}
test(S00){1,2}
test(S00){1=>2}
test(S00){x:=1}
test(S00){let(x:=1). x+1}				#TODO:FIX
test(N00){x}
test(S00){f(1):=2}
test(S00){f(x:int):int=x}
test(V10){f(x:int):char=x}
test(S00){for{}}
test(S00){for{1|2}=array{1,2}}
test(S00){fv(:any) := if(1) then 2 else 3}
test(S00){fv(:any) := if(1=2) then 2 else 3}
test(S00){fv(:any) := 2=if(1) then 2 else 3}
test(S00){fv(:any) := 3=if(1=2) then 2 else 3}
test(S00){fv(:any) := if(1+1) then 2+1 else 3+1}
test(S00){f(x:int):=x; f[3]=3}
test(D00){f()<decides>:int => f[]}					#TODO:10.4:EFF-DECIDES
test(S00){f():=3 => f[]=3}
test(F00){f():=3 => f[]=4}
test(D00){x:int => x=3}
test(U00){f():any => f[]='3'}
test(D00){f():int => f[]=3}
test(F00){f():int => f[]='3'}
test(QF00){x.blah,x:=1=2}
test(S00){for(x:=0|1) do nat[x]}
test(S00){for(x:=0|1) do nat[x]}
test(S00){function(x:=0|1){let(x){}}}
test(S00){for(x:=0|1){-x}}
test(S00){function(x:=0|1){-x}}
test(S00){for(x:=0|1) do :nat=x}		#TODO:???
test(S00){f(x:=0|1):nat=x}
#LA test(V04){type{(for|first){}}}
#LA test(V04){f(m:=let|for):=m(1){2}}
#LA test(V04){for(m:=let|for) do m(1){2}}
test(S00){f():type{int|int} => :f[]=3}	#TODO:???

# Error ordering.
test(N04){f(g():any):=g[]; f(g():=6)}
test(N04){f(x:int)<decides>:=x=3, f[x:int]}

# Open world + conditionals.
test(S00){fv(:any) := x:int=if(1) then 1 else 2}
test(S00){fv(:any) := x:int=if(1=2) then 1 else 2}
test(S00){a:int => :int=3}							#TODO:HIDING
test(S00){a:int => :int=a}							#TODO:HIDING
test(S00){a:int => :int=if(1) then 2 else 3}		#TODO:HIDING
test(S00){a:int => :int=if(1=2) then 2 else 3}		#TODO:HIDING
test(S00){a:int => if(a=1) then :int=2 else :int=3}	#TODO:HIDING
#test(S00){a:int => :int=if(a=1) then 2 else 3} #!! MUST FIX

#
# Runtime unification - maximum asynchronous progress.
#

# Shows we must not resolve a flexible future to an inflexible one unless it's a head.
test(QS00){allow{x:int=x; x=3; x=3}}
test(QR20){allow{x:int=x; x=3; x=5}}
test(QR03){allow{x:int=x; not (x=3,x=5)}}
test(QS00){allow{as:type{_,_}=(_,_); as=(3,4); as=(3,4)}}
test(QR20){allow{as:type{_,_}=(_,_); as=(3,4); as=(3,5)}}
#test(S00){xs:=(3,4); ys:=(3,4); if(allow{as:type{:int,:int}=as; as=xs; as=ys}) then 1 else false?}
#test(S00){xs:=(3,4); ys:=(3,4); if(allow{x:int=3}) then 1 else 2}

# Shows that when unifying a flexible variable with an inflexible array, we need
# to make a new flexible array.
test(QR03){a:=assume{_}; for{1|(a=3; a=4)}=array{_}} # because a=3 and a=4 both get stuck
test(QS00){a:=assume{_}; for{1|(b:=a; b=3; b=4)}=array{_}} # because b is flexible
test(QS00){a:=assume{_,_}; for{1|(b:=a; b=(3,4); b=(3,5))}=array{_}}

# Conditions lacking runtime semantics that we need to figure out.
# Shouldn't the verifier be rejecting both conditions as abstract?
#test(S00){x&y:any,x=y,if(x){x=1}}
#test(S00){x&y:any,x=y,if(x=y){x=1}}
#test(S00){x&y:int,x=y,if(x=y){x=1}}

#
# Comparable.
#

test(S00){comparable[3]}
#??test(S00){comparable[3,5]}
test(U00){a:any => comparable[a]}
test(S00){a:int => comparable[a]}
test(S00){a:comparable => comparable[a]}
test(U00){a:any => comparable[a,3]}
#test(S00){a:int => comparable[a,3]}
#test(S00){a:comparable => comparable[a,3]}

# This goes wrong because we have a decides context with Stops that holds effects.
#if{allow{:int}}

# Weird runtime comparisons.
#LA test(S00){decides=decides}
#LA test(F00){decides=fails}
#LA test(S00){array=array}
#LA test(U00){array=for}

# Runtime input plumbing.
test(S00){f():=1; f[]}
test(R00){allow{f():=allow{:nat}; f[]}}
test(S00){f():=3,f=f}
test(S00){f():=3,if{f=f}}

# Weird error.
#test(U00){operator'-'[3]}

# Depends on Abstraction propagation policy.
test(D00){f()<computes>:=a=b where a&b:int => f[]=f[]}		#TODO:10.4:CHECK-STRENGTH
test(S00){f()<computes>:type{a=b where a&b:int} => f[]=f[]}
test(D00){f():type{a=b where a&b:int} => f[]=f[]}
test(U00){c:int => f(c):={a=b where a:any,b:any} => f[3]}

# Analyze what is happening here that makes this work without op.mask.
test(S00){f(g():int):int => h():int => f[h]}
test(U00){f(g():int):int => h()<interacts>:int => f[h]}					#TODO:10.4:EFF-INTERACTS
test(QD00){f(g():int):int => h()<interacts>:int => assume<decides>{f[h]}}
test(QS00){f(g():int):int => h()<interacts>:int => assume{f[h]}}

# Code generation divergence across control flow analysis paths.
#LA test(S00){f(m:=let):=m(x:=2){x+1=3}}
#LA test(S00){f(m:=for):=m(x:=2){x+1}=array{3}}
#LA test(V04){type{(for|first){}}}
#LA test(V04){f(m:=let|for):=m(1){2}}
#LA test(V04){for(m:=let|for) do m(1){2}}
# Suspect not going wrong because these uses of macros create their own frames.
#test(R00){for(i:=0..1){m:any; for{}; if(i=0) then m=for; if(i=1) then m=for}}
#test(R00){for(i:=0..1){m:any; if(i=0) then m=for; m{}; if(i=1) then m=for}} # why not inconsistent ops across forks?
#test(R00){for(i:=0..1){m:any; if(i=1) then m=for; m{}; if(i=0) then m=for}} # why not inconsistent ops across forks?

# Testing operator'?'.
test(S00){type{3}?}
test(A00){int?}
test(S00){int?=3}
test(F00){int?='3'}

# Solving functions with inputs or multiple function confusion.
#test(F00){1=(2=>3)}
test(S00){1=>2}
test(S00){(H->F)(1):=2}
test(S00){(H->F)(1):=2}
#test(...){(G->F)(1):=2; G=(1=>2)} # Loops and not a bug: g is identity, so 1=>2 calls self.
test(S00){(G->F)(1):=2; H:=(1=>2); G=H}
test(S00){(G->F)(1):int; H:=(1=>3); G=H}
test(F00){(G->F)(1):=2; H:=(1=>3); G=H}		#NOTE:TODO
test(S00){h(1):=2 => {(H->F)(1):=2; H=h}}
test(A00){(H->F)(1):=2; H[1]} # Can tighten to S00 by implementing input_lambda_variable.
test(S00){f(g(1):=2):=7 => f[h(1):=2]}
test(U00){f(g(1):=2):=7 => f[h(1):=3]}
test(U00){f(g(1):=2):=7 => f[h(3):=2]}
test(S00){a->b:=function(1){2}; c:=function(1){2}; a=c}
test(S00){c:=function(1){2}; a=c; a->b:=function(1){2}}
test(A00){a->b:=function(:int){:nat}}
test(S00){a->b:=function(1){2}; c:=function(1){2}; a=c}
test(S00){a->b:=function(:int){2},c:=function(:int){2}; a=c}
test(S00){a->b:=function(1){:int}, c:=function(1){2}; a=c}
test(S00){a->b:=function(:int){:int}; c:=function(:int){2}; a=c}
test(S00){a->b:=function(:nat){:int}; c:=function(:int){3}; a=c}
test(S00){a->b:=function(){:int}; c:=function(){4}; a=c}
test(S00){a->b:=function(:nat){3}; c:=function(:int){3}; a=c}
test(S00){h->f:=function(){_}; g:=function(){3}; h=g}
test(S00){i->x:prefix'-',i=3,x=-3}
test(S00){i->x:=1}
test(S00){as->bs:=(1,2)}
#Broken by FunctionMacro UncommittableNegotiable
#test(S00){for(i->x:function(j:=3|5){j+1}){i,x}=((3,4),(5,6))}
#test(S00){(i->x,j->y):array{array{3,5}},i=0,j=1,x=3,y=5} # When we have tuple destructuring.
#Range-No-IterateStep test(F00){f(x:int):=x=function(1){2}}
#test(...){f(x:int):=x=2=function(1){2}} # Loops and not a bug: x unification identity maps function input to itself.
#Range-No-IterateStep test(F00){f(x:int):=x=function(1){2}=function(3){4}}

# Concatenation is janked.
#as:=array{a:int} => n:=Length[for{W->X:=(:as,a=1)}] where n=0 or n=1
#test(S00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=2 or n=3}}
#as:=array{:int} => n:=Length[for(a:as,a=1){1}] where n=0 or n=1
#as:=array{a:int} => bs:=for{W->X:=(:as,a=1)} where bs=array{} or bs=array{_}
#as:=array{a:int} => n:=Length[for{W->X:=(:as,a=1)}] where n=0 or n=1

# Testing operator'?'.
test(S00){type{3}?}
test(A00){int?}
test(S00){int?=3}
test(F00){int?='3'}

# Lack of function unification.
#if we make function_variable flexible, becomes X00: test(U00){(1=>2)=(1=>2)}

# Types and function resolves.
test(S00){f():=type{123} => 456}
test(S00){f():type{123} => f[]}
test(S00){f():type{g():int} => 2}

# Was broken by syntax.call not handling input properly.
test(F00){W->X:=array{w->x:=any[3]} where w=4}		# NOTE:???

# Was broken by native Length taking array<> rather than array<unificand>.
test(S00){as:=array{_} => Length[for{as[_]}]}

# Empty array previously didn't support input.
test(S00){a->b:=()}
test(S00){let(){2}}

# Conditions with range inputs.
test(S00){t:=type{a:int,b:if(a=1) then int else char}; t[1,3]; t[2,'x']; not t[1,'3']; not t[2,3]}

# Functions with complex effects.
test(S00){f()<decides>:int => f[] => 1}
test(S00){f(x:int)<decides>(y:int):int => f[3]}
test(S00){f(x:int)(y:int)<decides>:int => f[3]}
test(S00){f(x:int)<decides>(y:int)<decides>:int => f[3]}
test(D00){f(x:int)<decides>:type{g(y:int):int} => f[3]}
test(S00){f(x:int)<reads><reads><reads>:=x}

# Assuming input test (and, if function, capturing function strength).
test(D00){a:int => f(3)():=4 => f[a]}              # Input test assumes as strong as function Abstraction.
test(S00){g(f(3):=4):=5, a:int => g[f(a):=4] => 5} # Input test assumes as strong as caller.
test(U00){g(f(3):=4):=5, a:int => g[f(a):=4]}      # Input test doesn't assume otherwise.

# Assuming range:
test(S00){f(x:int)<decides>:=x=3, a:int => f[a] => a=3} # Range assumes as strong as caller (mod stage).
test(D00){f(x:int)<decides>:=x=3 => a:int => f[a]}      # Range doesn't assume otherwise.	#TODO:10.4:EFF-DECIDES
test(S00){f(x:int)<decides>:=x=3 => a:=3  => f[a]}

# Must Abstraction.FunctionDepth strength contribute to verifier_context strength?
# Can't find any example.
test(S00){a:int => f(:int)():type{nat[a]} => f[a]}
test(D00){a:int => f(:nat)():type{nat[a]} => f[a]}
test(S00){a:int => f(:int):type{g():type{nat[a]}} => f[a]}
test(D00){a:int => f(:nat):type{g():type{nat[a]}} => f[a]}

# Shows need for the tighter new FunctionMacro fails rules.
test(S01){g0(:int)<fails>:=3}
test(S00){g0(:int)<fails>:false}
test(S00){g0(:false):=3}
test(S00){f(g0(:int)<fails>:false):=4 => f(false)}
test(S00){f(g0(:int)<fails>:false):=4 => f(g1(x:false):=3)}
#test(P00){f(g0(:int)<fails>:false):=4 => f(g1(x:int):=3)}
test(S01){f(g0(:int)<fails>:=3):=4 => 5}

# Shows need for the tighter new FunctionMacro non-succeeds rules.
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:any     ):=3]}
test(QS00){f(g0(:int)<decides>:int):=3 => f[g1(x:rational):=3]}
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:int     ):=3]}
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:nat     ):=3]}
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:false   ):=3]}
#
#test(U00){f(g0(:int)<decides>:false):=3 => f[g1(x:any     ):=3]}
#test(U00){f(g0(:int)<decides>:false):=3 => f[g1(x:rational):=3]}
#test(U00){f(g0(:int)<decides>:false):=3 => f[g1(x:int     ):=3]}
#test(U00){f(g0(:int)<decides>:false):=3 => f[g1(x:nat     ):=3]}
test(S00){f(g0(:int)<decides>:false):=3 => f[g1(x:false   ):=3]}
#
test(S00){f(g0(-3)<decides>:int):=3 => f[g1(x:any     ):=3]}
test(S00){f(g0(-3)<decides>:int):=3 => f[g1(x:int     ):=3]}
#test(U00){f(g0(-3)<decides>:int):=3 => f[g1(x:nat     ):=3]}
test(S00){f(g0(-3)<decides>:int):=3 => f[g1(x:false   ):=3]}
#
test(S00){f(g0(:int)<decides>:=2):=3 => f[g1(x:any     ):=2]}
test(S00){f(g0(:int)<decides>:=2):=3 => f[g1(x:int     ):=2]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:nat     ):=2]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:=2      ):=2]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:false   ):=2]}
#
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:any     ):=3]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:int     ):=3]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:nat     ):=3]}
#test(U00){f(g0(:int)<decides>:=2):=3 => f[g1(x:=2      ):=3]}
test(S00){f(g0(:int)<decides>:=2):=3 => f[g1(x:false   ):=3]}
#
#test(U00){f(g0(x:int)<decides>:=nat[x]):=3 => f[g1(x:any     ):=x]}
#test(U00){f(g0(x:int)<decides>:=nat[x]):=3 => f[g1(x:int     ):=x]}
test(S00){f(g0(x:int)<decides>:=nat[x]):=3 => f[g1(x:nat     ):=x]}
#test(U00){f(g0(x:int)<decides>:=nat[x]):=3 => f[g1(x:=2      ):=x]}
#test(U00){f(g0(x:int)<decides>:=nat[x]):=3 => f[g1(x:false   ):=x]}

# Higher order functions.
test(U00){a:int => g(f()(x:int):=3):=4 => g[f1()(x:int):=a]}
test(S00){a:int => g(f()(x:int):=3):=4 => g[f1()(x:int):=a] => 3}
test(QS00){a:int => g(f()(x:int):=3):=4 => assume<succeeds>{g[f1()(x:int):=a]}}
test(QD00){a:int => g(f()(x:int):=3):=4 => assume<decides >{g[f1()(x:int):=a]}}
test(QD00){a:any => g(f()(x:int):=3):=4 => assume<decides >{g[f1()(x:int):=a]}}

# Higher order functions with arrays are broken.
test(S00){f(1=>2):=7 => f[1=>2]}
#test(S00){f(1=>2,3):=7 => f[1=>2,3]}
#test(S00){f(g(:int):int,3):=7 => f[any,3]}

# Stage needs significant work, including conceptually.
# If's LeftStep recalculation says nothing about stage, which is in an outer context.
# It's recalculated later, but by then it's too late for the 'if'.
# Perhaps 'if' case addresses this with negotiability.
# What about function range case?
#test(S00){f():any=3; if{f[]}}
#test(S00){f():int=3; if{f[]}}
#test(X00){f(x:int)<interacts>:int=3; if{f[2]}}
#test(X00){f(x:int)<interacts>:int=3; if{f(2)}}
#test(S00){if{x:int<decides>=3}}
#test(S00){a:int => if{x:int<decides>=a=3}}
#test(D00){f(a:int):=if(:int=a=3) then a=3}
#test(D00){f(a:int):=if(:int<decides>=a=3) then a=3}
#test(S00){if{x:int<decides>=3}}
#test(S00){if{x:int<decides>:false}}

# Function effects inclusion.
test(U00){f(g()<computes>:int):int => g()<interacts>:int => f[g]}	#TODO:10.4:EFF
test(U00){f(g()<computes>:int):int => g():int => f[g]}				#TODO:10.4:EFF
test(U00){f(g()<computes>:int):int => f[g():int=3]}
test(U00){f(g()<computes>:int):int => f[g():type{3}=3]}
test(P00){f(g(:int)<computes>:int):int => f(g(x:int):type{3}=3)}
test(S00){f(g():int):int => g():int => f(g)}
#
test(S00){f(g():int)<succeeds>:int => g()<succeeds >:int => f(g)}
test(P00){f(g():int)<succeeds>:int => g()<decides  >:int => f(g)}
test(P00){f(g():int)<succeeds>:int => g()<resolves >:int => f(g)}
test(P00){f(g():int)<succeeds>:int => g()<interacts>:int => f(g)}
#
test(S00){f(g()<decides >:int):int => g()<succeeds >:int => f(g)}
test(S00){f(g()<decides >:int):int => g()<decides  >:int => f(g)}
test(P00){f(g()<decides >:int):int => g()<resolves >:int => f(g)}
test(P00){f(g()<decides >:int):int => g()<interacts>:int => f(g)}
#
test(S00){f(g()<resolves>:int):int => g()<succeeds >:int => f(g)}
test(S00){f(g()<resolves>:int):int => g()<decides  >:int => f(g)}
test(S00){f(g()<resolves>:int):int => g()<resolves >:int => f(g)}
test(P00){f(g()<resolves>:int):int => g()<interacts>:int => f(g)}
#
test(P00){f(g()<computes>:int):int => g()<succeeds >:int => f(g)}
test(P00){f(g()<computes>:int):int => g()<decides  >:int => f(g)}
test(P00){f(g()<computes>:int):int => g()<resolves >:int => f(g)}
test(P00){f(g()<computes>:int):int => g()<interacts>:int => f(g)}
test(S00){f(g()<computes>:int):int => g()<computes >:int => f(g)}

# Strange.
test(S00){f(g(:int):int):int => f[g(x:int):int=x]}
test(S00){f(g(:int):int):int => f(g(x:int):int=x)}

# Supporting non-assuming flexibility contexts nested inside assuming flexibility contexts.
test(P00){f(1):=2 => f(3) => 4}

# Should be F00, perhaps by suppressing P00 when degenerate.
# This weights in against the rule: 'any head contradiction in the equation masks out succeeds'.
test(P00){f(x:int):=x => f(3)=5} # Should be F00.
test(F00){f(x:any):=x => f(3)=5}
test(F00){f(x:=3):=x => f(3)=5}
test(P00){f(x&y:int):=(y,x) => f(3,5)=(5,5)} # Should be F00.
test(P00){allow{x:false},operator'+'(x)}     # Should be S00.
test(P00){allow{x:false},operator'+'(x)=7}   # Should be S00.

# Array unification.
test(S00){(1,2)=(1,2)}
test(F00){(1,2)=(1,3)}
test(S00){f(x:int):=x => f(3)=3}
test(S00){a&b&c&d:int => (a,b)=(1,2) => nat[a]}
test(S00){f(x&y:int):=(y,x) => f(3,5)=(5,3)}
test(F00){f(x&y:int):=(y,x) => f[3,5]=(5,5)}
test(F00){f(x&y:int):=(y,x) => f[3,5]=(3,3)}
test(S00){a&b:int => c:=(a,b) => c=(a,b)}
test(D00){a&b:int => c:=(a,b) => c=(b,a)}
test(U00){a&b:int => c:any => f():type{c=(a,b)} => {c=(a,b)}}
test(S00){a&b:int => c:any => f():type{c=(a,b)} => {f[],c=(a,b)}}
test(S00){a&b:int => c:any => f():type{c=(a,b)} => {c=(a,b),f[]}}
test(S00){a&b&c&d:int => (a,b)=(1,2) => nat[a]}
test(S00){a&b&c&d:int => (a,b)=(c,d) => {a=c,b=d}}
test(S00){(a,2)=(1,2),a:int}
test(S00){a:int,(a,2)=(1,2)}
test(S00){(_,_)=(1,2)}

# Testing tests.
#LA test(F00){test(S00){2=3}}
#LA test(T01){test(F00){2}}

# Abstraction and reverse solving through function input.
#!!!test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]}
#!!!test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=3}
#!!!test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=3}
test(F00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=4}
#!!!test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=_}
#!!!test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][_]=3}

# Abstraction in array elements.
test(S00){f():=(:int) => 4}
test(S00){f():=array{3} => 4}
test(A00){f():=array{_} => 4}		#TODO:10.4:ARRAY-UNDERSCORE
test(S00){f():=array{:any} => 4}
test(S00){f():=array{:any} => f()[0]}
test(U00){f():=array{:any} => f()[0]=3}
test(S00){f():=array{:int} => 4}
test(S00){f():=array{:int} => f()[0]}
test(D00){f():=array{:int} => f()[0]=3}
test(S00){f():=(3,:int) => 4}
test(QS00){a:int => f(:nat):=(3,:int) => {allow{xs:=f[a]}}}
test(QS00){a:int => f(:nat):=(3,:int) => {allow{xs:=f[a]}; xs[1]}}
test(QD00){a:int => f(:nat):=(3,:int) => {allow{xs:=f[a]}; int[xs[1]]}}

#
# Functions.
#

# Function was unsound with PHAA, now PHAH.
test(S00){a:int => a=3 => x:type{a=3}=a}
test(V10){a:int => x:type{a=3}=a}
test(V10){a:int => x:type{a=3}=3}
test(V10){a:int => x:type{a=3}=a=3}
test(V10){a:int => x:type{a=3}=x}
test(V10){a:int => {allow<decides>{a=3}, f():type{a=3}=a=3}}
test(V10){a:int => if{a=3,f():type{a=3}=a=3}}
test(V10){a:int => {a=3, f():type{a=3}=a=3, f[]}}
test(V10){a:int => if{a=3, f():type{a=3}=a=3, f[]}}
test(S00){type{x:int,if{x=2}}}

# Interesting to consider. Was S00 before function PHAH.
# Could probably evolve rules to support this without breaking test(V10){x:int => if{x=3,f():type{x=3}=x=3}}
test(D00){x:int => if{x=3,f():=x=3}}

# Stage+Function is unsound regardless of PHAA unless we modulate its strength when it's run.
test(V10){a:int => {f():type{a=3}=a=3,f[]}} # Should refine to D00.

# Modulate staged expression multiplicity by assumed multiplicity.
#LA test(S02){assume{x:int<decides>=3}}
#LA test(QS00){f(a:int)<decides>:=assume{x:int<decides>=a=3}}
#LA test(QV10){a:int => if{x:int<succeeds>=a=3}}
#LA test(QV10){a:int => if{x:int<fails>=a=3}}
test(S00){f()<decides>:int => f[] => 123}
test(S00){f()<decides>:int => assume{f[]}}
test(S00){f()<decides>:int => if{f[]}}
test(U00){f()<resolves>:int => if{f[]}}
test(S00){f(a:int):=if(a=3) then a=3}
#LA test(S00){if{x:int<decides>=3}}
#LA test(S00){a:int => if{x:int<decides>=a=3}}
test(D00){f(a:int):=if(:int=a=3) then a=3}
#LA test(D00){f(a:int):=if(:int<decides>=a=3) then a=3}
#LA test(S00){if{x:int<decides>=3}}
#LA test(S00){if{x:int<decides>:false}}

# Assumption forks inside stage.
test(S00){f():type{int|nat} => {x:f[]=3}}
test(S00){f():type{int|int} => {x:f[]=-3}}
test(V10){f():type{int|nat} => {x:f[]=-3}}
test(V10){f():type{nat|int} => {x:f[]=-3}}
test(S00){f(x:=0|1):nat=x}
#test(S00){f(x:=0..1):=x, nat[f[1]]}
#test(S00){f(x:=0..1):nat=x, nat[f[1]]} # Functions must intersection-iterate over domain matches.
#test(V10){f(x:=-1..1):nat=x, nat[f[1]]}

# Proper explore.UnpromotableNegotiable effects propagation.
test(S00){f(g(:int)<decides>:int):int => f[false]}
test(U00){f(g(:int)<succeeds>:int):int => f[false]}
test(S00){f(g(:int)<fails>:int):int => f[false]}

# Have we introduced unsoundness by masking out h but allowing granular :t<x>? Seems not.
test(S00){f(g(:any)<decides >:any):=3 => f[false]}
test(U00){f(g(:any)<succeeds>:any):=3 => f[false]}

#
# Delimited domain_context.
#

# Delimited domain_context is inadequate with <decides> functions given that
# we intermingle solving and abstraction.
#test(D00){a:int => f(x:nat)<decides>:type{nat[a]} => f[a]}
#test(D00){a:int => f(x:int)<decides>:=let(nat[x]) do :type{nat[a]} => f[a]}
#test(D00){a:int => f(x:nat)<decides>:type{nat[a]}<succeeds> => f[a]}
#test(D00){a:int => f(x:int)<decides>:=let(nat[x]) do :type{nat[a]}<succeeds> => f[a]}

#
# Effects abstraction checks.
#

#LA test(S00){f(x:int)<succeeds>:int<succeeds>=3}
#LA test(V10){f(x:int)<succeeds>:int<succeeds>=x=3}
#LA test(V10){f(x:int)<succeeds>:int<succeeds>:false}
#LA test(S00){f(x:int)<decides >:int<succeeds>=3}
#LA test(V10){f(x:int)<decides >:int<succeeds>=x=3}
#LA test(V10){f(x:int)<decides >:int<succeeds>:false}
#LA test(S00){f(x:int)<decides >:int<decides>=3}
test(QS00){f(x:int)<decides >:int=x=3}
test(QS00){f(x:int)<decides >:int:false}
#LA test(QS02){f(x:int)<decides >:int<resolves>=3}
#LA test(QS00){f(x:int)<resolves>:int<succeeds>=3}
#LA test(QS00){f(x:int)<resolves>:int<decides>=3}
#LA test(QS00){f(x:int)<resolves>:int<resolves>=3}

test(S00){f(x:int)<decides >:=x=2}
test(D00){f(x:int)<fails   >:=x=2}
test(S01){f(x:int)<fails   >:=2}
test(S01){f(x:int)<diverges>:=2}
test(F00){f(x:int)<diverges>:false}
#test(S00){f(x:int)<diverges>:false:f(x)}

#
# Soundness of nested function application.
#

# Nested functions.
test(S00){f()():=3 => f[]}
test(S00){f(5)(6):=7 => f[5]}

# Contingency stacking. Shows f[a] strength is the weaker of f[a] domain match and f op_function.
test(U00){a&b:any => f(a=b)():=a=b => f[a=b]}
test(D00){a&b:int => f(a=b)():=a=b => f[a=b]}
test(S00){a&b:int => f(a=b)():=a=b => if{f[a=b]}}
test(D00){a&b:int => f(a):=a=3 => f[3]}
test(D00){a&b:int => f(a,b):=a=3 => f[3,b]}
test(S00){a&b:int => f(a):=4 => g(b):type{f[3]} => g[b]}
test(U00){a&b:int => f(g():any):=3 => f[g()<decides>:=a=b]}
test(U00){a&b:int => f(g():any):=3 => f[g():=a=b]}
test(U00){a&b:int => f(g():any):=g[] => f[g()<decides>:=a=b]}
test(U00){a&b:int => f(g():any):=g[] => f[g():=a=b]}
test(D00){a&b:int => f(a=b):=a=b => f[a=b]}
test(D00){a&b:int => f(a=b):=a=b => f[a=b]}
test(D00){a&b:int => f(a=b)():=a=b => f[a=b]}
test(D00){a&b:int => f(a=b)():=a=b => f[a=b]}
test(D00){a&b:int => f(:nat):=a=b => f[a]}
test(D00){a&b:int => f(:nat):=a=b => {f[a],a=b}}
test(D00){a&b:int => f(:nat):=nat[a] => f[a]}
test(D00){a&b:int => f(:nat)():=nat[a] => f[a][]}
test(S00){a&b:any => c:int => f(:int):type{a=b} => {f[c], a=b}}
test(D00){a&b:any => c:int => f(:nat):type{a=b} => {f[c], a=b}}
test(D00){a&b:int => f(a)(b):type{a=3} => f[3][b]}
test(D00){a&b:int => f(a)(b):type{b=3} => f[a][3]}
test(D00){a&b:int => g(b):=4 => f(a):=g => f[3][b]}
test(S00){a&b:int => g(b):type{a=3} => f(a):=g => f[3][b]}
test(D00){a&b&c:int => f(:nat):=b=c => f[a]}
test(U00){a:int => b&c:any => f(:nat):=b=c => 7}		#TODO:10.4:WHY
test(D00){a:int => b&c:any => f(:nat):type{b=c} => f[a]} # not being forced?
test(D00){a:int => b&c:any => f(:nat):type{b=c} => {f[a],b=c}} # same
test(U00){a:any => b&c:any => f(:nat):=b=c => f[a]}
test(D00){a&b&c:int=>f(x:nat)():=nat[a]=>f[a][]}

test(D00){a:int => f(0)():int => f[a]}
test(S00){a:int => f(0)():int => f[a] => 4}
test(S00){a:int => f(0)():int => f[a][] => 4}

test(D00){a:int => f(b:nat):=nat[b] => f[a]}
test(D00){a:int => f(:nat):=nat[a] => f[a]}
test(D00){a:int => f(:int):=nat[a] => f[a]}		#TODO:SKIP:ASSUME-FAIL
test(S00){a:int => f(:int):type{nat[a]} => f[a]}
test(D00){a:int => f(6)():=7 => f[a]}
test(D00){a:int => f(b:nat)():=nat[b] => f[a]}
test(D00){a:int => f(b:nat)():=nat[b] => f[a][]}
test(D00){a:int => f(:nat)():type{a=8} => {r:=f[a]; r[]}} # Was D00, why not S00 under NEWABS?
test(D00){a:int => f(:nat):type{a=8} => f[a]}
test(D00){a:int => f(8):type{a=8} => f[a]}
test(S00){a:int => f(:nat):type{a=8} => f[a] => f[a]}
test(S00){a:int => f(:nat)():=8 => r:=f[a] => r[]}
test(S00){a:int => f(:nat)():type{a=8} => r:=f[a] => 9}
test(S00){f(g():int):=g[] => h():nat => i:=f[h] => nat[i]}
test(S00){a:int => f(x:nat):int => g(x:int):type{f[x]} => g(a)}
test(U00){a&b:int => f(g():int):=g[] => {h()<decides>:=a=b,f[h]}}
test(P00){a&b:int => f(g():int):=g() => {h()<decides>:=a=b,f(h)}}

# Contingent assumptions.
test(U00){a&b:any => f(a=b):=a=b => f[a=b]}
test(D00){a&b:int => f(a=b):=a=b => f[a=b]}
test(U00){a&b:any => f(a=b):=a=b => if{f[a=b]}}
test(S00){a&b:int => f(a=b):=a=b => if{f[a=b]}}
test(U00){a:any => f(nat[a]):=nat[a] => f[a]}
test(D00){a:int => f(nat[a]):=nat[a] => f[a]}
test(U00){a:any => f(nat[a]):=nat[a] => if{f[a]}}
test(S00){a:int => f(nat[a]):=nat[a] => if{f[a]}}
test(D00){a&b:int => f():type{a=b} => a=b}
test(S00){a&b:int => f():type{a=b} => {f[],a=b}}
test(S00){a&b:int => f():type{a=b} => {a=b,f[]}}
test(D00){a&b:int => f(a):type{a=b} => f[b]}
test(D00){a&b:int => f(a=b):type{a=b} => f[a]}
test(D00){a&b:int => f(a=b):type{a=b} => f[b]}
test(D00){a&b:int => f(a=b):type{a=b} => f[a=b]}
test(D00){a&b:int => f(a):type{a=b} => {x:=b,f[x]}}
test(D00){a&b:int => f(a):type{a=b} => {f[x],x:=b}}		#TODO:10.4:CHECK-STRENGTH:TYPE
test(A00){f(x:int):int => {x:=f[x],first{int[x]}}}
test(S00){a:int => f(x:nat):int => if{f[a]=3}}
test(S00){a:int => f(x:int):int => if{int[f[a]]}}
test(S00){a:int => f(x:nat):int => if{int[f[a]]}}
test(S00){a:int => f(x:nat):int => if{nat[f[a]]}}
test(U00){a:any => f(x:nat):int => if{int[f[a]]}}
test(U00){a:int => f(x:nat):any => if{int[f[a]]}}
test(S00){a&b:int => f(x:nat):int => if{f[a]=f[b]}}
test(S00){a:int => f(x:nat):int => if{nat[f[a]]}}
test(U00){a:int => f(x:nat):any => if{f[a]=1}}
test(S00){a:int => f(x:nat):int => if{f[a]=1}}
test(S00){a:int => f(x:nat):type{let(y&z:any){y=z}} => if{f[a]}}
test(A00){f(:int):int => for{f[x:int]=x}}
test(D00){a&b&c&d:int => f(a):type{c=d} => g(c):=a=b => {f[x],g[d],x:=b}}	#TODO:10.4:CHECK-STRENGTH:TYPE
test(D00){a:int => {x:=x,x=a,x=3}}
test(D00){a:int => {x:=x,x=3,x=a}}
test(D00){a:int => {x:int,x=a,x=3}}
test(D00){a:int => {x:int,x=3,x=a}}

# Eta conversion with inflexible calls.
test(U00){p&q:int => as:=array{a:any} => f(p):=as[0]=3 => f[q]}
test(D00){p&q:int => as:=array{a:any} => f(p):type{as[0]=3} => f[q]}
test(U00){a:int => b:any => f(x:int):=int[x] => if{a=b,f[b],int[b]}}

test(U00){a&b:int => f(:int):any => m(:nat):type{a=b} => {f[a]=f[b],m[a]}}
test(D00){a&b:int => f(:int)<computes>:any => m(:nat):type{a=b} => {f[a]=f[b],m[a]}}
test(U00){a:int => f(x&y:any):=x=y => {f[a,a],a=2}}
test(D00){a:int => f(x&y:any):type{x=y} => {f[a,a],a=2}}
test(D00){c:int => f(c):type{a=b where a:any,b:any} => f[3]}
test(U00){a:int => f(x:nat):any => f[a]=f[a]}
test(D00){a:int => f(x:nat)<computes>:any => f[a]=f[a]}
test(U00){a&b:int => f(x:nat):any => eq(x&y:any):=x=y => {x:=f[a],y:=f[b],eq[x,y]}}
test(D00){a&b:int => f(x:nat):any => eq(x&y:any):type{x=y} => {x:=f[a],y:=f[b],eq[x,y]}}
test(D00){a&b:int => f(:int)<computes>:any => m(:nat):=a=b => {f[a]=f[b],m[a]}}
test(D00){a&b:int => f(:int)<computes>:any => m(:nat):type{a=b} => {f[a]=f[b],m[a]}}

# Contingent assumptions.
test(U00){a:any => f(x:nat):any => f[a]}
test(D00){a:int => f(x:nat):any => f[a]}
test(U00){a:any => f(x:nat):any => f[a]=f[a]}
test(S00){a&b:int => f():type{a=b} => f[]}
test(S00){a&b:int => f():type{a=b} => f[] where a=b}
test(S00){a&b:int => f():type{a=b} => a=b where f[]}
test(U00){a&b:any => f(a=b):type{a=b} => f[a=b] where a=b}
test(D00){a&b:int => f(a=b):type{a=b} => f[a=b] where a=b}
test(D00){a&b:int => f(x:nat):any => {x:=f[a],y:=f[b]}}
test(U00){a&b:int => f(x:nat):any => if{x:=f[a],y:=f[b],x=y}}

# FunctionMacro flaw where op::iterate generates promotable context containing assumption.
test(S00){a&b:int => f(g(4 where a=b):=5):=6 => f[h(4):=5]}
test(D00){a&b:int => f(g(4 where a=b):=5):=6 => {f[h(4):=5],a=b}}

#
# Effects abstraction.
#

# Effects abstraction.
test(V10){:false=3}
test(V10){:false=3=4} # V10 is a weird choice of error, prefer F00
test(S00){f(x:int):int=x}
test(V10){f(x:int):int=x=3} # V10 is a weird choice of error, prefer D00
test(V10){f(x:int):int=3=4} # V10 is a weird choice of error, prefer F00
test(S00){f(x:int)<decides>:int=3}
test(S00){f(x:int)<decides>:int=x=3}
test(S00){f(x:int)<decides>:int=3=4}
test(S00){f()<decides>:=3 => f[]}
test(D00){f()<decides>:int=3; f[]}	#TODO:10.4:EFF-DECIDES
test(D00){f()<decides>:int => f[]}	#TODO:10.4:EFF-DECIDES
test(U00){f():any => f[]=7}
test(U00){f()<decides>:any => f[]=7}
test(QS02){f()<decides>:int<succeeds> => f[]}
test(D00){f()<decides>:int => f[]}	#TODO:10.4:EFF-DECIDES (same as l659)

# Effects based subtyping and calling.
test(S00){f(a:int,b:int):=1,f(3,5)}
test(S00){f(g(:int)          :int):int => f(g(x:int):=x)}
test(S00){f(g(:int)          :int):int => f(g(x:int):int=x)}
test(P00){f(g(:int)<computes>:int):int => f(g(x:int):type{x}=x)}
test(S00){f(g(:int)<computes>:int):int => f(g(x:int):=x)}
test(S00){f(g(:int)          :int):int => f(g(x:int)<computes>:=x)}
test(S00){f(g(:int)<computes>:int):int => f(g(x:int)<computes>:=x)}
test(S00){f(g(:int)          :int):int => f(g(x:int)         :=x)}
test(S00){f(g(:int)          :int):int => f(g(x:int)<decides>:=x)}
test(U00){f(g(:int)          :int):int => f[g(x:int)<decides>:type{x}=x]}
test(P00){f(g(:int)          :int):int => f(g(x:int)<decides>:type{x}=x)}
test(S00){f(g(:int)<decides> :int):int => f(g(x:int)         :=x)}
test(S00){f(g(:int)<decides> :int):int => f(g(x:int)<decides>:=x)}
test(S00){f(g(:int)<decides>:int)<decides>:=g[3] => f(g(x:int):=x)}
test(D00){f(g(:int)<decides>:int)<decides>:=g[3] => g(x:int)<decides>:int => f[g]}	#TODO:10.4:EFF-DECIDES
test(S00){f(g(:int)<decides>:int)<decides>:=g[3] => f[g(x:int):int=4]}
test(D00){f(g(:int)<decides>:int)<decides>:=g[3] => f[g(x:int)<decides>:int=4]}
test(P00){f(g(:int)<decides>:int)<decides>:=g[3] => f(g(x:int)<decides>:int=4)}

# Equalty inference and effects abstraction.
test(U00){f(x:any)            :any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(U00){f(x:any)<converges >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(S00){f(x:any)<computes  >:any => f(3)=f(3)}
test(U00){f(x:any)<reads     >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(U00){f(x:any)<writes    >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(U00){f(x:any)<varies    >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(U00){f(x:any)<transacts >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
test(S00){f(x:int)<computes><diverges >:any => if{f[3]=f[3]}}
test(S00){f(x:int)<computes><succeeds >:any => if{f[3]=f[3]}}
test(U00){g(f(x:any)<throws_any>:any)<throws_any>:=f(3)=f(3)}	#TODO:FUN-OUT-EQ-EFF-THROWS
test(S00){g(f(x:any)<computes><throws_any>:any)<throws_any>:=f(3)=f(3)}
test(U00){g(f(x:any)<interacts           >:any)<interacts>:=f(3)=f(3)}	#TODO:FUN-OUT-EQ-EFF-INTERACTS
test(S00){g(f(x:int)<computes><fails     >:any) := if{f[3]=f[3]}}
test(S00){g(f(x:int)<computes><decides   >:any) := if{f[3]=f[3]}}
test(U00){g(f(x:int)<computes><resolves  >:any) := if{f[3]=f[3]}}
#!!error order only test(S02){g(f(x:int)<computes><iterates  >:any) := if{f[3]=f[3]}}
#!!error order only test(S02){g(f(x:int)<computes><abstracts >:any) := if{f[3]=f[3]}}

# Abstract functions: effects declaration says what effects the assumed range may have.
test(S00){f(x:int)           :int => f[3]}
test(D00){f(x:int)<decides  >:int => f[3]}		#TODO:10.4:EFF-DECIDES
test(U00){f(x:int)<resolves >:int => f[3]}		#TODO:10.4:EFF-RESOLVES
test(P00){f(x:int)<decides  >:int => f(3)}
test(P00){f(x:int)<resolves >:int => f(3)}
test(S00){f(x:int)<interacts>:int => 3}
test(S00){f(x:int)<interacts>:int=3; f[3]}
test(S00){f(x:int)<interacts>:int=3; f(3)}
#test(X00){f(x:int)<interacts>:int=3; if{f(3)}}

# Functions with abstracts|iterates effects.
test(S00){f(x:int):=x}
test(F00){f(x:int):false}
test(D00){f(x:int):=x=3}
test(X00){f(x:int):=2|3}
test(A00){f(x:int):int}

test(S00){f(x:int)<succeeds>:=x}
test(F00){f(x:int)<succeeds>:false}
test(D00){f(x:int)<succeeds>:=x=3}
test(X00){f(x:int)<succeeds>:=2|3}
test(A00){f(x:int)<succeeds>:int}

test(S00){f(x:int)<decides>:=x}
test(S00){f(x:int)<decides>:false}
test(S00){f(x:int)<decides>:=x=3}
test(X00){f(x:int)<decides>:=2|3}
test(A00){f(x:int)<decides>:int}

# Unallowed multiplicity.
test(S02){f()<iterates>:=1}
test(S02){f()<abstracts>:=1}

# Function calling with multiplicity.
test(S00){f(x:int):=x}
#
test(S00){f(x:int)<succeeds>:=x,f(3)}
#
test(S00){f(x:int)<decides>:=x,f(3)}
test(F00){f(x:int)<decides>:false,f[3]}	#NOTE:FP
test(P00){f(x:int)<decides>:false,f(3)}	#NOTE:FP
test(S00){f(x:int)<decides>:=x=3}
test(S00){f(x:int)<decides>:=x=3,f[3]}
test(S00){f(x:int)<decides>:=x=3,f(3)}
test(F00){f(x:int)<decides>:=x=3,f[4]}
test(P00){f(x:int)<decides>:=x=3,f(4)}

#
# Stage.
#

# Stage and circular arrays.
#test(V10){a:any=(1,a)}

# Stage and circular functions.
#test(S00){a:any=f():=a}

# Staging.
test(S00){:int=2}
test(V10){:int='c'}
test(V10){x:int=(2=3)}
test(V10){a:int => x:int=(a=2)}
test(N00){x:int=y}
test(S00){x:int=3}
test(S00){x:int=3,int[x]}
test(D00){x:int=3,x=3}
test(D00){x:int=3,x=2}
test(D00){x:int=3,nat[x]}
test(S00){x:nat=3,int[x]}
test(F00){x:int => x=false}
test(F00){x:int=3,x='c'}
test(F00){x:int=3,x=false}	#NOTE:ASCRIPTION

# Staging with recursive references.
test(S00){y:=(3,y[0])}
test(S00){x:any=y:=7}
test(S00){x:any=y:=(3,y[0])}

# Stage recursion and circularity.
test(S00){x:int => y:int=x}
test(V10){x:int=x}
test(S00){x:int=3,y:int=4}
test(S00){(x:int=3) where int[x]}
test(S00){(x:int=3) where y:int=x}
test(S00){x:int=3,y:int=x}
test(S00){x:int=y,y:int=3}
test(S00){x:int=y,y:int=3,int[x],int[y]}
test(V10){x:int=y,y:int=x}

# Stage recursion and arrays.
test(S00){xs:type{3,:int}=(3,5)}
test(S00){xs:type{3,:int}=(3,5); xs}
test(S00){xs:type{3,:int}=(3,5); xs[0]}
test(S00){xs:type{3,:int}=(3,5); xs[0]=3}
test(F00){xs:type{3,:int}=(3,5); xs[0]=4}
test(S00){xs:type{3,:int}=(3,5); xs[0]=3}
test(F00){xs:type{3,:int}=(3,5); xs[0]=4}

# Recursive reference out of stage. Require tighter <abstracts>.
#test(V10){x:int=3 where int[x]}
test(S00){x:type{3}=x}
#test(S00){x:int=3 where x}
#test(S00){xs:type{3,:int}=let(xs) do (3,5)}
#test(S00){xs:type{3,:int}=(xs[0],7)}
#test(S00){xs:type{3,:int}=(xs[0],xs[0])}

# Stage recursion: Checking context doesn't have priveleged information that's available outside.
# We could tighten this in the future with a different structure.
test(S00){xs:type{:int,:int}=ys:=(3,ys[0])}
test(S00){xs:type{:int,:int}=ys:=(ys[1],3)}
test(V10){xs:type{:int,:int}=(3,xs[0])}
test(V10){xs:type{:int,:int}=(xs[1],3)}

# Staging with function abstraction.
test(S00){f(x:prefix'-'):=x}
test(S00){f(x:prefix'-'):=x,f[7]=-7}
test(F00){f(x:prefix'-'):=x,f[7]=7}
test(S00){x:prefix'-'=3}
test(U00){x:prefix'-'=3,x=0}          # Will tighten when natives have type approximations.
test(U00){x:prefix'-'=3,x=3}          # Will tighten when natives have type approximations.
test(U00){x:prefix'-'=0,x=0}          # Will tighten when natives have type approximations.
test(U00){x:prefix'-'=3,x=-3}         # Will tighten when natives have type approximations.
test(D00){a&b:int => {a=b => 1; a=b}} # Verifying Uncommittable aren't committing.

# Stage and type{..}.
test(S00){x:type{3}=3}
test(V10){x:type{3}=4}
test(V10){x:type{3}='c'}
test(S00){x:type{3}=x}
test(S00){x:type{:int}=3}
test(S00){x:type{:int}=4}
test(V10){x:type{:int}='c'}
test(S00){type{3,4}[3,4]}
test(S00){x:function(y:=(3,4)){y}=(3,4)}
test(S00){x:type{3,4}=(3,4)}
test(S00){x:type{3,4}=(x[0],x[1])}
test(V10){x:type{3,:int}=(x[0],x[1])}
test(V10){x:type{:int,:int}=(x[0],x[0])}
test(S00){x:type{a:int,a+1}=(3,4)}
test(V10){x:type{a:int,a+1}=(3,5)}
test(S00){x:type{a:int,a+1}=(3,_)}
test(V10){x:type{a:int,a+1}=(_,5)}
test(S00){x:type{_}=3}
test(S00){x:type{_}=(3,3)}
test(S00){x:type{_,_}=(3,3)}
test(S00){x:type{}=()}
test(S00){x:type{_}=3}
test(S00){x:any=(3,3)}
test(S00){x:any=array{3}}

# Stage value should take input because it may be part of a layered approximation.
test(S00){x:prefix'-'=3}
test(V10){x:int:prefix'-'=3} # Will tighten S00 natives have type approximations.

# Conditions trying to cheat stage.
test(S00){a:int => :int=if(8=9) then 3 else 4}
test(S00){a:int => :int=if(8=9) then 3 else 4}
#!!test(S00){a:int => :int=if(a=3) then 3 else 4}
#Explore-UncommittableNegotiable test(V10){x:int=if(x=3) then 3 else 4} # Could accept this.
#Explore-UncommittableNegotiable test(V10){x:int=if(x=3) then 4 else 5} # Must reject this.
#!!test(V10){x:int=y,if(x=1) {y=1} else {y=2},y:int} # PHAA x:int=y mustn't prove int[x].

# Stage with conditions and resolves-detection across contexts.
test(V10){a:any=if(b:=a){b}}
test(V10){a:any=if(b:=a){b}}

# Stage functions and running.
test(S00){f():int=3}
test(V10){f():nat=-3}
test(S00){f():int=0,int[f[]]}
test(D00){f():int=0,nat[f[]]}

# Stage evaluation basics.
test(S00){f(x:int):int=3; f[4]}
test(S00){f(x:int):int=3; int[f[4]]}
test(D00){f(x:int):int=3; f[4]=5}
test(F00){f(x:int):int=3; f[4]='x'}
test(S00){f(x:int):int=x; f[4]}
#test(S00){f(x:int):int=x; assert{f[4]=4}}
#test(RXX){f(x:int):int=x; assert{f[4]=5}}
#needs non-<converges> special case: test(S00){f(x:int):int=f[x]}
#test(S00){f(x:int):int=f[x]; f[3]} //need error code to say "don't run".

# Stage evaluation equality inference.
test(A00){f(x:int)<computes>:int; f[3]=f[3]}
test(S00){f(x:int)<computes>:=(:int=4); f[3]=f[3]}
#NEWABS test(S00){f(x:int)<computes>:=(:int,4) => f[3][0]=f[3][0]}
#Broken prior to NEWABS test(S00){f(x:int)<computes>:=(:int=3,4); f[3][0]=f[3][0]}

# Stage paraconsistency.
test(V10){f(x:int):false=3}
#needs non-<converges> special case: test(S00){f(x:int):false=f[x]}
#test(A00){f(x:int)<converges>:false=f[x]}

# Stage soundness required being contingent on domain.
test(S00){a&b:int => x:=a=b => :type{a=b}=x}
test(D00){a&b:int => f(a=b):type{a=b} => f[_]}
test(S00){a&b:int => f(x:=a=b):type{a=b}=x}

# Stage in abstract contexts with inputs.
# Rejecting this is not wrong, because we're checking 3 against inflexible g-input.
# Stage right-hand expression does need that input in case it's of the form :t.
#!!Need to sort out proper input handling.
#test(V10){f(g():int=3):=4}
#test(V10){g():int=3 => 4}
#test(V10){g():type{3}=3 => 4}
#test(V10){a&b:int => f(x:=a=b):type{a=b}=x => f[_]}
#test(V10){a&b:int => f(x:=a=b):type{a=b}=x => 3}

# Stage nesting.
test(S00){x:int:int=3}

#
# Relevance.
#

test(A00){1 where a:any} # Could tighten to S00.	#TODO:10.4:WHY
test(A00){1 where a:=a}  # Could tighten to S00.	#TODO:10.4:WHY

#
# Cycles.
#

# Convergence of circular function definitions.
test(S00){f(1):=f}
test(S00){f(f):=1}
test(S00){f(f):=f}
test(S00){f(x:=f):=x}
test(S00){f(1):=f,f}
test(S00){f(1):=f,f[1]}
test(S00){f(1):=f,f(1)}
test(S00){f(f):=1,f[f]}
test(S00){f(f):=1,f(f)}
test(S00){f(f):=f,f[f]}
test(S00){f(f):=f,f(f)}
test(S00){f(x:=f):=x,f[f]}
test(S00){f(x:=f):=x,f(f)}

# Bidirectional calls.
test(S00){f(x:any):=x      => f[3]=3}
test(F00){f(x:any):=x      => f[3]=4}
test(S00){f(x:any):=x      => f[3]=_}
test(S00){f(x:any):=x      => f[_]=4}
test(A00){f(x:any):type{x} => f[_]=4}

# Cyclic arrays.
test(A00){as:=(1,as)}
test(A00){as:=(1,(2,as))}
test(A00){as:=(1,(as,2))}
test(A00){as:=array{as}}
test(A00){as:=(1,as),bs:=(1,bs)}
test(A00){as:=(1,as),bs:=(1,bs),as=bs}
test(F00){as:=(1,as=2)}
test(U00){a:any => as:=(1,as=a)}

#
# Allowed effects checking.
#

# Call effects allowability.
test(X00){if{Print(123)}}
test(X00){for{Print(123)}}
test(S00){f(123)<interacts>:=Print(456)}
test(X00){Print[123] => 456}
test(X00){Print(123) => 456}
test(X00){if{Print[123]}}
test(X00){for{Print[123]}}
test(X00){Print(123) => 456}
test(S00){g(f():=3)<interacts>:=Print(3)}

# Must IterateStep commit clear AllowFx and inherit parent?
test(QS00){a:any => b:int => allow{x:=a=b} where if{x=3}}
test(QA00){allow{x:any} where if{x}}

# Requires abstract treatment of natives and effects.
test(S00){f(g()<interacts>:any):=3}
test(S00){f(g(:any)<interacts>:any):=4 => f[Print]}
test(S00){h(f(g(:any)<interacts>:any)<interacts>:=g[4])<interacts>:=f[Print]}
test(S00){h(f(g(:any)<interacts>:any)<interacts>:=g[4])<interacts>:=f(Print)}
test(U00){h(f(g(:any):any)<interacts>:=g[4])<interacts>:=f[Print]}
test(P00){h(f(g(:any):any)<interacts>:=g[4])<interacts>:=f(Print)}
test(X00){h(f(g(:any)<interacts>:any):=g[4])<interacts>:=f[Print]}
test(X00){f(x:int where Print(x)):=x}

#
# Uniform function range flexibility until :t or :t=v.
#

test(S00){f():=3 => f[]}
test(S00){f():=3 => f[]=3}
test(S00){f():=3 => int[f[]]}
test(S00){f():int => f[]}
test(D00){f():int => f[]=3}
test(S00){f():int => int[f[]]}
test(D00){f():int => nat[f[]]}

test(S00){f(x:int)<decides>:=x=3, f[:int]}
test(F00){f(x:int)<decides>:=x=3, f[:char]}	#NOTE:FP
test(D00){f(x:int)<decides>:=x=3, y:int=>f[y]}
test(S00){f(x:int)<decides>:=x=3 => f[:int]}
test(F00){f(x:int)<decides>:=x=3 => f[:char]}
test(D00){f(x:int)<decides>:=x=3 => y:int=>f[y]}	#TODO:10.4:EFF-DECIDES

test(S00){x:int => int[x]}
test(D00){x:int => nat[x]}
test(D00){x:int => char[x]} # could be F00
test(S00){f():int => f[]}
test(S00){f():int => int[f[]]}
test(D00){f():int => nat[f[]]}
test(D00){f():int => char[f[]]} # could be F00
test(S00){x:int=3; int[x]}
test(D00){x:int=3; nat[x]}
test(D00){x:int=3; char[x]} # could be F00
test(S00){f():int=3; int[f[]]}
test(D00){f():int=3; nat[f[]]}
test(D00){f():int=3; char[f[]]} # could be F00
#test(V10){f():int=3 => int[f[]]} # could be S00
#test(V10){f():int=3 => nat[f[]]} # could be D00
#test(V10){f():int=3 => char[f[]]} # could be F00
test(F00){f():=2=3 => 4}							#TODO:10.4:ASSUME-FAIL:RANGE
test(D00){f(x:int):=x=3 => 4}						#TODO:10.4:ASSUME-FAIL:RANGE
test(S00){f():type{2=3} => 4}
test(S00){f(x:int):type{x=3} => 4}

# Testing ':' attributes.
test(QS02){:int<succeeds>}
test(QS00){x:int,x=3}
test(QS02){x:int<succeeds>,x=3}
#LA test(S00){:int<succeeds>=3}
#LA test(S02){:int<contravariant>=3}
#LA test(S02){:int<decides>=3}
#LA test(S00){:int<computes>=3}
test(QS00){:void=3}
test(QS00){:void=false}
test(QX00){:void<computes>=Print("Hello")}
test(QV10){a:int => :int=a=3} # Would prefer D00 to prioritize above V10.

#
# Total Functions.
#

# Verify-time divergent functions without stage. Disabled obviously.
#test(S00){f():=f[]}

# Runtime divergent functions with stage.
#test(S00){f():any=f()}
#test(R01){f():any=f(); Err(); f()}
#test(S00){f():int=f()}
#test(R01){f():int=f(); Err(); f()}
#test(S00){f()<converges>:={:int=f()[1],:int=f()[0]}}
#test(R01){f()<converges>:={:int=f()[1],:int=f()[0]}; Err(); f()}
#test(S00){f()<converges>:={:int=f()[1],:int=7}}
#test(R01){f()<converges>:={:int=f()[1],:int=7}; Err(); f()}
#test(A00){f()<converges>:any=f()}
#test(A00){f()<converges>:int=f()}
#test(A00){f()<converges>:={:int=f()[1],:int=f()[0]}}
#test(A00){f()<converges>:={:int=f()[1],:int=7}}
#test(A00){f(x:int)<converges>:int=if(x=0) then 0 else f(0)}
#test(S00){f(x:int)<converges>:=if(x=0) then 0 else :int=f(0)}

# Cyclic functions.
test(S00){f():=f; f[]=f}
test(S00){f(f):=3}
test(S00){f(f):=3; f[f]=3}
test(S00){f(f):=f}
test(S00){f(f):=f; f[f]=f}

# Cyclic functions with stage.
test(S00){f():any=f}
test(S00){f(f):any=3}
test(S00){f(f):any=f}

#
# Function Non-Escalation.
#

test(D00){a&b:int => f(a=b):=3 => f[a=b]}
test(P00){a&b:int => f(a=b):=3 => f(a=b)}
test(D00){a&b:int => a=b where f():=a=b}
test(S00){a&b:int => f(:any):type{a=b} => f[a=b]}
test(D00){a&b:int => f(a=b):type{a=b} => f[a=b]}
test(F00){f(:any):false => x:false where f[x]}

#
# Function Input Checking.
#

# Function use of visible assumption to check inputs.
# should be done in non-visible !AssumeParentSucceeds context.
test(D00){a&b:int => f(g(4          ):=5):=6 => f[h(4          ):=5] where a=b}
test(S00){a&b:int => f(g(4 where a=b):=5):=6 => f[h(4          ):=5]}
test(U00){a&b:int => f(g(4          ):=5):=6 => f[h(4 where a=b):=5]}
test(D00){a&b:int => f(g(4 where a=b):=5):=6 => f[h(4          ):=5] where a=b}
#test(U00){a&b:int => f(g(4          ):=5):=6 => f[h(4 where a=b):=5] where a=b}
test(S00){a&b:int => f(g(a=b        ):=5):=6 => f[h(a=b        ):=5]}

#
# Function fixed points.
#

# Broken when we introduced Tarjan algorithm expanding Resolved, fixed by eliminating it.
test(S00){f(a:any):any => x:=f[2]}
test(S00){f(a:any):any => x:=f(2)}
test(A00){f(a:any):any => x:=f[x]}
test(P00){f(a:any):any => x:=f(x)}
test(U00){f(a:any):any => x:=f[x]=2}
test(S00){f(a:any):=a  => x:=f[x]=2}
test(S00){f(a:any):=(3,a) => x:=f(2)}
test(A00){f(a:any):=(3,a) => x:=f[x]}	#NOTE:CHECK
test(P00){f(a:any):=(3,a) => x:=f(x)}
#
test(S00){f(a:int):int => x:=f[2]}
test(S00){f(a:int):int => x:=f(2)}
test(A00){f(a:int):int => x:=f[x]}
test(P00){f(a:int):int => x:=f(x)}
test(U00){f(a:int):int => x:=f[x]=2} # Could be D00.
test(S00){f(a:int):=a  => x:=f[x]=2}
test(S00){f(a:int):=(3,a) => x:=f(2)}
test(P00){f(a:int):=(3,a) => x:=f(x)}
#
test(S00){f(a:comparable):comparable => x:=f[2]}
test(S00){f(a:comparable):comparable => x:=f(2)}
test(A00){f(a:comparable):comparable => x:=f[x]}
test(P00){f(a:comparable):comparable => x:=f(x)}
test(U00){f(a:comparable):comparable => x:=f[x]=2} # could be D00
test(S00){f(a:comparable):=a         => x:=f[x]=2}
test(S00){f(a:comparable):=(3,a) => x:=f(2)}
#loops test(P00){f(a:comparable):=(3,a) => x:=f(x)}

#
# Simple tests.
#

# Trivial tests.
test(S00){3}
test(S00){3=3}
test(F00){3=5}
test(S00){-3}
test(S00){if{123}=123}
test(F00){if{123}=456}
test(S00){if{2=3}}
test(F00){if{2=3}=3}
test(S00){if(1){2}else{3}=2}
test(S00){if(1=2){3}else{4}=4}
test(S00){if(1=2){3}else if(3){4} else{5}=4}
test(S00){if(1=2){3}else if(3=4){5} else{6}=6}
test(S00){if(x:=1){x=1}}
test(S00){y:=2 where y=2}
test(S00){y:=2,y=2}
test(S00){function(1){2}}
test(F00){f:=function(1){2},f[3]}	#NOTE:ASSERT
test(S00){int[123]}
test(S00){int(123)}
test(S00){int[123],int(123)}
test(S00){f:=function(x:int){x},f[1]}
test(S00){f:=function(x:int){x},f(1)}
test(S00){f:=function(x:=int[x]){x},f(1)}
test(S00){f:=prefix'-',p:=3,f(p)}
test(S00){(f:=prefix'-')(p:=3),f,p}
test(F00){false[0]}
test(F00){:false}		#NOTE:???
test(S00){int[0]}
test(F00){int['x']}
test(F00){3 => {4=5}}
test(S00){void()=array{}}
test(S00){if{2=3}=false}
test(S00){if(x:=1){y:=2,y=2}}
test(S00){--3=3}
test(S00){not -3=3}

# Shows RecalculateFx-when_cast_context using equation EffectiveFx pollutes
# reasoning about inflexibles and constants. Probably need fails|diverges-reachability
# rather than equation co-inhabitance.
#test(R20){allow<fails>{expect<succeeds>{-3}=4}}
#test(R20){allow<fails>{expect<succeeds>{-3} where 3=4}}

# More complex arrays.
test(S00){y:=2,y=2}
test(S00){as:=array{2,3,4} => as[1]}
test(S00){as:=array{2,3,4} where as[1]}
test(S00){as:=array{2,3,4} => as[1]=3}
test(S00){as:=array{3} where as[0]=3}
test(S00){as:=array{2,3,4} where as[1]=3}
test(S00){array{2,3,4}[1]=3}
test(S00){as:=array{3} where as[0]=3}
test(S00){as:=(3,5) => as[0]=3}
test(F00){as:=(3,5) => as[0]=4}
test(F00){as:=(3,5) => as[0]=5}

# Trivial array flow tests.
test(S00){123}
test(F00){123=456}
test(S00){int[3]}
test(F00){int['3']}
test(A00){array{_}}
test(S00){array{3}}
test(S00){array{3}=array{3}}
test(F00){array{3}=array{4}}
test(A00){array{_}[0]}
test(S00){array{3}[0]}
test(S00){array{3}[0]=3}
test(F00){array{3}[0]=4}
test(S00){a:=2,a=2}
test(F00){a:=2,a=3}
test(S00){as:=array{3} => as[0]}
test(S00){as:=array{3} => as[0] where as[0]}
test(S00){as:=array{3} => as[0]=3}
test(F00){as:=array{3} => as[0]=4}
test(U00){as:=array{_} => as[0]=4}
test(S00){array{1,2}}
test(S00){array{2,3,4}[1]=3}
test(F00){array{2,3,4}[5]}

# Calling.
test(S00){-1}
test(S00){operator'+'[1,2]}
test(S00){1+2}
test(S00){1/2}
test(F00){1/0}
test(F00){1+2=5}	#NOTE:ASSERT
test(S00){Length(1,2)=2}
test(S00){void("XYZ")=array{}}
test(S00){let(x:=1){y:=2,x+y}}

# Effective array assumptions.
test(S00){as:any => as=array{_}    => as[0]}
test(U00){as:any => as=array{_}    => as[0]=4}
test(F00){as:any => as=array{3}    => as[0]=4}
test(S00){as:any => as=array{3}    => as[0]=3}
test(D00){as:any => as=array{:int} => as[0]=3}

# Rebuilding tests.
test(S00){123}
test(F00){3=4}
test(S00){()}
test(S00){()=()}
test(A00){array{x:=x,y:=y}}
test(S00){array{3}}
test(S00){array{3,5}}
test(S00){array{3=3,5}}
test(F00){array{3=4,5}}
test(S00){array{3,4}=array{3,4}}
test(F00){array{3,4}=array{5,4}}
test(A00){(x:=x,y:=y)=(a:=a,b:=b)}
test(S00){a:=(3,4) => a}
test(S00){a:=(3,4) => (3,4)}
test(F00){a:=(3,4,5) => a=(3,5)}
test(F00){a:=(3,4) => a=(3,5)}
test(U00){a:=a => a=(3,4)}
test(D00){a:=(:int,:int) => a=(3,4)}
test(F00){3=(4,5)}
test(F00){(_,_)=(_,_,_)}
test(S00){a:int => {x:=3,assume{x=a},u:=x=a}}
test(R20){x:=3,assume{x=5},x=5}
test(R20){x:=2,assume{x=3}}
test(A00){assume{x=(_,_,_)},x:=(_,_,_)}
test(U00){a:any => a=(2,3)}
test(R20){assume{x=3},x:=4}

# Previously broken.
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]=cs[0]}

# Wildcard identifier '_'.
test(S00){_=3}
test(S00){3=_}
test(S00){_=3=_}
test(F00){3=_=4}
test(S00){(3,_)=(_,4)}

# Anonymous definitions '_'.
test(S00){_:=123}
test(S00){_:=123,_:=456}
test(S00){_(x:int):=int[x]}
test(D00){_(x:int):=nat[x]}

# Totally array-free tests.
test(S00){x:=2 where x=2}
test(F00){x:=2 where x=3}
test(S00){x:int where x=2}
test(F00){x:int where x='X'}

# Simple arrays.
test(S00){2,3}
test(S00){2,2}
test(S00){2=2,3=3}
test(F00){2=2,3=4}

# New unification bring-up.
test(S00){1}
test(S00){1=1}
test(F00){1=2}
test(S00){-1}
test(A00){x:=x}
test(S00){x=y,x:=1,y:=1}
test(F00){x=1,x:=2}
test(F00){x:=2,x=1}
test(F00){x=y,x:=1,y:=2}
test(S00){x:any => x}
test(S00){x:any => x=x}
test(U00){x&y:any => x=y}
test(D00){x&y:int => x=y}
test(S00){x:=x => x=1 => x=1}
test(S00){x&y:any => x=y => x=y}
test(QS00){x&y:any => {assume<succeeds >{x=y},x=y}}
test(QD00){x&y:any => {assume<decides  >{x=y},x=y}}
test(QU00){x&y:any => {assume<abstracts>{x=y},x=y}}
test(S00){if{123}}
test(S00){x&y:int => if{x=y}}
test(QS02){x&y:int => if{expect<abstracts>{x=y}}}
test(QD00){x&y:int => if{expect<succeeds>{x=y}}}
test(QS00){x&y:int => if{z:=x=y,expect<succeeds>{z}}}
test(QD00){x&y:int => if{z:=x=y,expect<succeeds>{z=x}}}
test(QD00){x&y:int => if{z:=x=y,expect<succeeds>{z=y}}}
test(F00){array{1}=array{2,3}}
test(QD00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=a}}}
test(QD00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{a=c}}}
test(QD00){a&b:int => if{x:=a=b  ,expect<succeeds>{x=a}}}
test(QD00){a&b:int => if{x:=a,x=b,expect<succeeds>{x=a}}}
test(QS00){a&b:int => if{x:=a,a=b,expect<succeeds>{x=a}}}
test(QS00){a&b:any => {x:=assume<succeeds >{y=a},y:=assume<succeeds >{x=b}}}
test(QD00){a&b:any => {x:=assume<decides  >{y=a},y:=assume<decides  >{x=b}}}
test(QU00){a&b:any => {x:=assume<abstracts>{y=a},y:=assume<abstracts>{x=b}}}
test(QU00){a&b:any => {x:=assume<abstracts>{y=a},y:=assume<abstracts>{x=b}}}
test(QS00){a&b:int => {x:=a,y:=b,assume<succeeds>{x=y}}}
test(QS00){a&b:int => {x:=a,y:=b,assume<succeeds>{x=y},a=b}}
test(QS00){a&b:int => {x:=a,y:=b,assume<succeeds>{x=y},x=y}}
test(QS00){a&b:int => if{x:=a,y:=b,assume<succeeds>{x=y},x=y,expect<succeeds>{x=y}}}
test(S00){a&b:int => {x:=b,assume{x=a}}}
test(S00){a&b:int => {x:=a=b,assume{x=a}}}
test(QD00){a&b:any => {x:=a=b,assume<decides>{x=a}}}
test(S00){a&b:int => {x:=a=b,assume{a=b}}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {f[a,b],a=b}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {f[a,a],a=b}}
test(QS00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=c}}}

# Components and cycles.
test(S00){x:=assume{y:=assume{z:=x}}}
test(QS00){assume<succeeds>{123}}

#
# The 'type' macro.
#

# Type.
test(S00){type{3}}
test(S00){type{:int}}
test(N00){type{x}}
test(S00){type{3}[3]}
test(F00){type{3}[4]}	#NOTE:ASSERT
test(S00){type{:int}[3]}
test(S00){type{:int}[4]}
test(F00){type{:int}['c']}
test(S00){type{:prefix'-'}[3]}
test(S00){type{:prefix'-'}[3]=-3}
test(F00){type{:prefix'-'}[3]=3}
test(S00){type{3,4}[3,4]}
test(F00){type{3,4}[3,5]}	#NOTE:ASSERT

#
# Non-Escalating WhenCastStep.
#

# WhenCastStep escalated FailTargetStep when assuming based on non-flow-based head visibility.
test(F00){assume{nat[_]}=-7}
test(F00){assume{int[_]}='c'}
test(F00){assume{nat[_]}='c'}
test(U00){x:=assume{y:=y},x=false}
test(F00){x:=assume{int[_]},x='c'}
test(F00){x:=assume{:int},x='c'}
test(F00){x:=assume{:int},x=false}
test(D00){x:=assume{int[_]} where x=3}
test(F00){x:=assume{r:=3} where x='$'}
test(F00){x:=assume{int[_]},x='c'}
test(QF00){x:int => {(3,5)[x] where allow<decides>{x=9}}}
test(S00){x:=nat['c'] => x='d'}
#Range-No-IterateStep test(S00){x:=nat[-3] => x='c'}

# General escalation risk.
test(R20){assume{x:int   where false[]}}
test(D00){assume{x:int                }=7}
test(D00){assume{x:int   where x+2=3  }=1}
test(D00){assume{x:int   where x+2=3  }=7} # D00 because of paraconsistency.
test(D00){assume{x:int   where 8=9    }=7} # D00 because of paraconsistency.	#TODO:10.4:ASSUME-FAIL
test(D00){assume{x:int   where false[]}=7} # D00 because of paraconsistency.	#TODO:10.4:ASSUME-FAIL
test(R20){assume{x:false where false[]}=7} # Succeeds because variable contradicts.
test(R20){assume{x:false              }=7}
test(S00){a:int => assume{x:false where a=3}=7}
test(D00){a:int => assume{x:int   where a=3}=7}
test(D00){a:int => assume{x:int   where a=false}=7} # D00 because of paraconsistency.

# We need flow-independent WhenCastStep heads to make assumptions effective.
test(S00){x:int => x=3 => x+1=4}
test(F00){x:int => x=3 => x+1=5}

#
# Paraconsistency:
# Any expression that verifies assuming x:t should verify assuming x:false since false<=t.
#

# Paraconsistency.
test(U00){assume{x:any   where 3=4}=5}	#TODO:10.4:ASSUME-FAIL
test(D00){assume{x:int   where 3=4}=5}	#TODO:10.4:ASSUME-FAIL
test(R20){assume{x:=5    where 3=4}=5}
test(F00){assume{x:=6    where 3=4}=5}	#TODO:10.4:ASSUME-FAIL
test(R20){assume{x:false where 3=4}=5}

# Reification failure not captured by shape and escalation.
test(F00){nat['x']}
test(F00){nat[-3]}
test(F00){x:int => false[x] where assume{x=3}}
test(R20){x:=2 where assume{x=3}}
test(S00){x:=3 where assume{x=3}}
test(F00){x:false  where assume{x=3}}
test(F00){x:=2=3   where assume{x=4}}
test(F00){x:=2=3=4 where assume{x=3}}
test(R20){x:=2=3   where assume{x=3}}
test(F00){x:false  where assume{nat[x]}}

# Head paraconsistency.
test(F00){2=3}
test(R20){assume{2=3}}
test(F00){a:int => {a=2,assume{a=3}}}
test(F00){assume{2=3},4=5}			#TODO:10.4:ASSUME
test(F00){assume{false[]},4=5}
test(F00){a:=2,assume{a=3},4=5}		#TODO:10.4:ASSUME

# Don't let diverges overwhelms fails.
test(R20){x:=2,assume{x=3}}
test(S00){0 => Err("Hello")};
test(R01){Err()};
test(R01){Err("Hello")};
test(F00){Err(),false[]}
test(F00){false[],Err()}
test(F00){0 => {Err("Hello"),2=3}};
test(F00){0 => {2=3,Err("Hello")}};

# "Contradiction is contained at projections" was the former title of this section.
test(S00){as:=(7,8=9) => as=2}
test(S00){as:=(7,8=9) => as=(_,_)}
test(S00){as:=(7,8=9) => as[0]}
test(F00){as:=(7,8=9) => as[0]=6}				#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(S00){as:=(7,8=9) => as[0]=7}
test(S00){as:=(7,8=9) => as[0]=8} # questionable due to constant degeneracy
test(S00){as:=(7,8=9) => as[0]=9} # questionable due to constant degeneracy
test(F00){as:=(7,8=9) => as[0]=10}				#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(S00){as:=(7,8=9) => as[1]=2 where 2=3}
test(F00){as:=(7,8=9) => as[0]=2}				#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(F00){as:=(7,8=9) => as where 2=3}			#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(F00){as:=(7,8=9) => as[0] where 2=3}		#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(F00){as:=(2,3=4) => as where 8=9}			#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(F00){as:=(7,8=9) => as=(_,_) where 2=3}	#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG
test(F00){as:=(7,8=9) => as[1] where 2=3}		#TODO:SKIP:ASSUME-FAIL:CHECK-TUPLE-ARG

# Failing and contradictory parameters to native functions.
test(S00){x:false => -x}
test(S00){x:false => -x=7}
test(S00){x:false => x+1}
test(S00){x:false => 1+x}
test(S00){x:false => x+1=7}
test(S00){x:false => 1+x=7}
test(S00){x:false => operator'+'[x]}
test(S00){x:false => operator'+'[x]=7}
test(S00){x:false => operator'+'(x)}
test(S00){x:false => operator'+'(x)=7}
test(QR20){allow{x:false}}
test(QF00){allow{x:false},prefix'-'[x]}
test(QR20){x:=allow{assume{:false}}}
test(QR20){x:=allow{assume{:false}},prefix'-'[x]}
test(QF00){allow{x:false},-x}
test(QF00){allow{x:false},-x=7}
test(QF00){allow{x:false},x+1}
test(QF00){allow{x:false},1+x}
test(QF00){allow{x:false},x+1=7}
test(QF00){allow{x:false},1+x=7}
test(QF00){allow{x:false},operator'+'[x]}
test(QF00){allow{x:false},operator'+'[x]=7}

# Conditions.
test(S00){if(0=1) then 2 else 3}
test(S00){if(0=1) then Print(2) else Print(3)}
test(S00){if(0=1) then Print("T") else Print("F")}
test(S00){f(x:=(0|1)):={5};  Print(for{:f})}

# Syntax change implications.
test(S00){for{0..0..3}=array{0,0,1,0,1,2,0,1,2,3}}

# Effects specifiers.
#LA test(X30){f()<1>:=2}

# Formerly bad error prioritization.
test(P00){f(1):=2 => f(3)}

# Calling contradictory functions.
test(S00){xs:false => xs[0]}
test(S00){xs:=1=2 => xs[0]}
test(S00){xs:=(0,1)=2 => xs[0]}
test(S00){xs:=2=(0,1) => xs[0]}
test(S00){xs:=(0,1)=2 => xs[0]=3}
test(S00){xs:=(0,1)=2 => xs[3]}
test(S00){xs:=(0,1)=2 => xs[3]=4}
test(S00){f():type{2=3} => f[]}
test(F00){f(x:false,y:int):=false[y]}
test(C00){f(x:false,y:int):=y[0]}
test(S00){f(x:false,y:int):=int[x]}
test(S00){f(x:false,y:int):=comparable[x]}
test(S00){f(x:false,y:int):=any[x]}
test(S00){f(x:false,y:int):=x[0]}
test(S00){f(x:false,y:int):=x(0)}

# The empty array.
test(S00){x:false => false[x]}
test(S00){x:false => false(x)}
test(S00){f(x:false,y:int):=false[x]}

# Calling contradictory arrays and functions produces contradictory result.
test(U00){a&b:any => (a,1)=2 => b=3}			#TODO:SKIP:ASSUME-FAIL
test(D00){a&b:comparable => (a,1)=b => a=3}
test(S00){f():=2 => f=3 => f[]=4}
test(U00){a:any => (a,1)=2 => a=3}				#TODO:SKIP:ASSUME-FAIL

# Uninhabited parameters to types.
test(S00){x:false => any[x]}
test(S00){x:false => int[x]}
test(S00){x:false => comparable[x]}
test(S00){x:false => int[x]}
test(S00){x:false => int(x)}
test(S00){f():false => f[]}
test(S00){f():false => f()}

# Conditions and contradiction.
test(S00){if{x:=1=2,if{x=3}}}
#test(S00){f(x:false):=if(x=1){1}else{2}=3}
test(D00){a&b:int => f():false => {a=b,if{a=b} else f()}}
test(D00){a&b:int => f():false => {if{a=b} else f(),a=b}}
test(S00){a&b:int => f():false => f()}
test(S00){a&b:int => f():false => if{a=b} else f()}
test(S00){if{x:false,if{x=3}}}

# Should any multiplicity be allowed under a contradictory condition?
# Was S00, now F00, need axiomatic semantics to weigh in.
test(F00){f(x:false):=if(x){1}else{2}=3}

# Inference in assumptions must not circularly escalate the strength of weaker inferences.
test(QF00){x:any => allow{assume<decides>{x=3}} where x=4}
test(QD00){x:any => allow{assume<decides>{x=3}} where x=3}
test(QD00){x:any => allow{assume<decides>{x=3}} where assume{x+1} where x=3}

# S00 is correct for now, but should be disallowed when operator'+' properly overloads:
test(QS00){x:any => allow{assume<decides>{x=3}} where assume{x+1}}
test(QS00){x:any => allow{assume<decides>{x=3}} where assume{x+1} where x+1=4}
test(QS00){x:any => allow{assume<decides>{x=3}} where assume{x+1=4}}

# Deep array effects.
test(S00){as:=(2,3=4) => as=(_,3)}
test(S00){as:=(2,3=4) => as=(_,4)}
test(S00){as:=(2,3=4) => as=(_,5)}
test(F00){as:=(2,3=4) => as=(7,_)}		#TODO:SKIP:ASSUME-FAIL
test(F00){as:=(2,3=4) => as[0]=7}		#TODO:SKIP:ASSUME-FAIL
test(S00){as:=(2,3=4) => as[1]=3}
test(S00){as:=(2,3=4) => as[1]=5}
test(F00){as:=(2,3=4) => as[5]}			#TODO:SKIP:ASSUME-FAIL

# Succeeding unifications and divergence.
test(F00){:false}
test(F00){f():false}
test(R01){Err("yo")=1}
test(S00){f(x:=Err[]):=x=1}
test(S00){f(x:=Err()):=x=1}

# Uninhabited container parameter.
test(S00){x:false => (1,2)[x]}
test(S00){x:false => (1,2)[x]=3}
test(QF00){(1,2)[x]   where allow{x:false}}
test(QF00){(1,2)[x]=3 where allow{x:false}}

# Uninhabited dot expressions.
test(QN05){int.sym}
test(QS00){f(x:=1=2):=x.blah}
test(QS00){f(x:false):=x.blah}
test(QN05){f(x:false,y:int):=y.blah}
test(QS00){f(x:false,y:int):=x.blah}
test(QS00){as:=(1,2)=3 => as.blah}
test(QS00){as:=(1,2)=3 => as[1].blah}
test(QF00){x:=1=2,x.blah}
test(QF00){x.blah,x:=1=2}
test(QF00){x:=allow{1=2},x.blah}
test(QF00){x.blah,x:=allow{1=2}}
test(QF00){(1=2).blah}

# Contradiction isn't contained at containers.
test(S00){as:=(2,3=4) => as=(2,5)}
test(S00){as:=(2,3=4) => as=(3,5)}
test(S00){as:=(2,3=4) => as=(2,_)}
test(S00){as:=(2,3=4) => as=(3,_)}
test(S00){as:=(2,3=4) => as=(1,2)}
test(S00){as:=(2,3=4) => as=7}
test(S00){as:=(2,3=4) => as=as}
test(S00){as:=(2,3=4) => as[1]=7}
test(S00){as:=(2,3=4) => as=7}
test(S00){as:=(2,3=4) => as where 2=3}
test(S00){as:=(2,3=4) => 2=3}
test(S00){as:=(2,3=4) => 3=4}
test(F00){as:=(2,3) => 2=3}
test(S00){as:=(2,3=4) => as[1]=7}
test(F00){as:=(2,3=4) => 2=5}		#TODO:SKIP:ASSUME-FAIL
test(F00){(a:=2,b:=3=4) => a=7}		#TODO:SKIP:ASSUME-FAIL

# Prior problems due to constant degeneracy.
test(S00){array{7,8} where if{1=0 where array{7,9}}}
test(F00){f(x:int)<computes>:=x => f[7]=8}
test(F00){f(x:int):=x => f[7]=8}
test(D00){a:int => f(x:int)<computes>:=x => f[7]=a}
test(D00){a:int => f(x:int):=x => f[7]=a}

# Head contradiction must be independent of visibility for assumptions to be effective and confluent.
test(C00){x:any => x[0]}
test(C00){x:int => x[0]}
test(S00){x:any => assume{x=(3,4)} where x[0]}
test(S00){x:int => assume{x=(3,4)} where x[0]}
test(S00){x:any => y:=x where assume{y=(3,4)} where x[0]}
test(S00){x:int => y:=x where assume{y=(3,4)} where x[0]}
test(S00){x:any => y:=x where assume{x=(3,4)} where y[0]}
test(S00){x:int => y:=x where assume{x=(3,4)} where y[0]}
test(S00){x:any => y:=x where assume{y=(3,4)} where y[0]}
test(S00){x:int => y:=x where assume{y=(3,4)} where y[0]}

# Tests that broke in the past due to reification atom race.
# These fork because reify stalls rather than handling paraconsistency, causing our intermediate arrays to expand.
test(F00){x:=x => f():=x=7 => 3=0 where f[]}
test(F00){x:=x => f():=x=7 => 3=9 where f[]}
test(F00){x:int => f():=x=7 => {if(x=6){9}else{3}=9,f[]}}
test(F00){x:int => f():=x=7 => {if(x=6){9}else{3}=0,f[]}}

# Contradictory variables.
test(S00){f(x:false):=x=1}
test(S00){f(x:false):=x=2}
test(S00){f(x:false):=x=1=2}
test(S00){f(x:=int['c']):=x=1}
test(F00){f(x:=int[3  ]):=x=1}
test(S00){f(x:=int[g(3):=4]):=x=1}
test(S00){f(x:=(3,5)[7]):=x=1}
test(S00){f(x:=first{2=3}):=x=1}
test(S00){f(x:=1/0):=x=1}
test(F00){f(x:=(3,5)[0]):=x=1}
test(U00){f(x:false,y:any):=y=1}
test(U00){f(x:false,y:any):=y=2}
test(F00){f(x:false,y:any):=y=1=2}

# Previously broken.
test(S00){f(7):=8,f[7]}
test(S00){f(7):=8,f[7]=8}
test(F00){f(7):=8,f[7]=9}
test(D00){a:int => {f(7):=8,f[7]=a}}

# Were strangely broken due to constant degeneracy.
test(S00){function(1 where as:=3){as}[1]=3}
test(F00){function(1 where as:=3){as}[1]=4}
test(F00){function(1 where as:=array{10,11,12}){as}[1]=(10,11,12,13)}
test(F00){function(1 where as:=array{10,11,12}){as}[1]=(13,14,15)}
test(F00){function(1 where as:=array{10,20,30}){as}[1]=(10,10,10)}
test(F00){function(1 where as:=array{1,2,3}){as}[1]=(1,1,1)}
test(F00){function(1 where as:=for{x:=1|2|3}){as}[1]=(1,1,1)}
test(S00){function(1 where as:=for{x:=1|2|3}){as}[1]=(1,2,3)}

# Synchronous contradictory variables.
test(F00){2=3}
test(R20){x:=2,assume{x=3}}
test(S00){a&b:int => {x:=a=b,assume{x=a}}}
test(S00){x:=2=3 => x}
test(S00){x:=2=3 => x=2}
test(S00){x:=2=3 => x=3}
test(S00){x:=2=3 => x=4}
test(R20){x:=2=3,assume{x=3}}
test(S00){x:false => 1}
test(S00){x:false => 1,1}
test(N00){x:false => y}
test(N00){x:false => y,1}
test(S00){x:false => x}
test(S00){x:false => x=1}
test(S00){x:false => x=1,1}
test(S00){x:=2=3 => 1}
test(S00){x:=2=3 => 1,1}
test(S00){x:=2=(3,4) => 1}
test(S00){x:=2=(3,4) => 1,1}
test(N00){2=3 => x}
test(N00){2=3 => x,1}
test(F00){92=93 => 94=95}			#TODO:SKIP:ASSUME-FAIL
test(F00){92=93 => 94=95,91}		#TODO:SKIP:ASSUME-FAIL
test(F00){92 => 4=5}
test(F00){92 => 94=95,1}

# Asynchronous contradictory variables.
test(S00){y:=2,x:=1=y => x=3}
test(S00){x:=1=y => x=3,y:=2}

# Contradictory targets resulting from operation failure.
test(S00){x:=false[] => x=3}
test(S00){x:=false[] => x=4}
test(S00){x:=int[3] => x=3}
test(F00){x:=int[3] => x=4}
test(S00){x:=int['x'] => x=4}
test(S00){x:=int['x'] => x='x'}
test(S00){x:=1..0 => x='x'}
test(S00){x:=1/0 => x=7}
test(D00){f(x:int):int => a:=f[2] => a=2}
test(D00){f(g():=3):int => a:=f[h():=3] => a=4}
test(S00){x:=first{0} => x=0}
test(F00){x:=first{0} => x=1}
test(S00){x:=first{:false} => x=0}
test(F00){not 3}
test(S00){x:=(not 3) => x=0}
test(S00){x:=not 3 => x=0}

# Ranges under contradictory domains.
test(S00){1=2 => 0}
test(S00){1=2 => 1=2}
test(F00){1=2 => 3=4}				#TODO:SKIP:ASSUME-FAIL
test(S00){f(1=2)<interacts>:=Print(3)}

# Vacuous heads.
test(S00){x:=2=3 => x}
test(S00){x:=2=3 => x,1}
test(S00){x:=2=(3,4) => x}
test(S00){x:=2=(3,4) => x,1}

# Vacuous array unification.
test(S00){as:=(10,20) => bs:=(11,20) => as=bs => as=bs}
test(S00){as:=(10,20) => bs:=(11,20) => as=bs => as(0)=bs(0)}
test(S00){as:=(10,20) => bs:=(11,20) => as=bs => as(1)=bs(1)}

# Decidability in the presence of uninhabited variables.
test(QS00){a&b:int => if{x:=a=b,expect<succeeds>{x}}}
test(QS00){if{x:=2=3,expect<succeeds>{x}}}
test(S00){x:=let(false[]){3} => x}
test(S00){x:=let(false[]){3} => x=3}
test(F00){x:=let(false[]){3} => x=4}
test(S00){x:=7=9 => x}
test(S00){x:=7=9 => x}
test(S00){x:=7=9 => x=7}
test(S00){x:=7=9 => x=9}
test(S00){x:=7=9 => 7=9}
test(F00){x:=7=9 => 6=8}		#TODO:SKIP:ASSUME-FAIL
test(F00){x:=7=9 => 6=8}		#TODO:SKIP:ASSUME-FAIL

# No loops.
test(S00){f():=2 => f=2 => f[]=3}

# Consider future axiomatic semantics.
#test(S00){as:=(_,_) => x:=as[7] => as[7]}
test(S00){as:=(_,_) => x:=as[7] => as[7]=x}
test(F00){as:=(_,_) => x:=as[7] => as[8]}				#TODO:SKIP:ASSUME-FAIL
#test(F00){as:=(_,_) => x:=as[7] => as[8]=x}
test(R20){assume{x=3},x:=4}
#test(R20){assume{x=(_,_,_)},x:=(_,_)}
#test(R20){assume{x=(_,_,_)},x:=(_,_)}

#
# Flow variable strength and shape.
#

test(QS00){allow<abstracts>{x:any} where 1}
test(QA00){allow<abstracts>{x:any} where (0,x)}
test(QA00){f(:any):any => allow<abstracts>{x:any} where f[x]}
test(QP00){f(:any):any => allow<abstracts>{x:any} where f(x)}

# Flow variable strength.
test(S00){a&b:int => f(:any):type{a=b} => f[a=b]} # D00 with variable strength.
test(F00){f(:any):false => x:false where f[x]}    # F00 with variable strength.
test(S00){a&b:int => assume{a=b}}
test(QS00){a:int => allow{x:=a=3} where assume{x}}
test(QR20){allow{x:=3,y:false} where assume{x=y}}
test(QR20){allow{x:=3,y:false where x=y        } where x}
test(QR20){allow{x:=3,y:false where assume{x=y}} where x}

# Contemplating shape flow.
test(QS00){a&b:int => allow{x:=a=b} where x}
test(QS00){a&b:int => allow{x:=a=b} where any[x]}
test(QD00){a&b:int => allow{x:=a=b} where int[x]}
test(QF00){allow{x:false} where x}
test(QF00){allow{x:false} where any[x]}
test(QF00){allow{x:false} where false[x]}
test(QF00){allow{x:false} where int[x]}

#
# Effects verification macros.
#

# Verify.
test(QS00){expect<succeeds>{1}}
test(QF00){expect<succeeds>{1=2}}
test(QS00){expect<succeeds>{1}=1}
test(QF00){expect<succeeds>{1}=2}
test(QF00){if{expect<succeeds>{1=2}}}
test(QF00){if{expect<succeeds>{1=2}}}
test(QS00){if{expect<decides>{1=2}}}
test(QS00){if{expect<fails>{1=2}}}
test(QF00){if{expect<succeeds>{false[]}}}
test(QS00){f(x:int):=expect<succeeds>{x},f[3]=3}
test(QF00){f(x:int):=expect<succeeds>{x},f[3]=4}

# Verify asserting in failed contexts.
test(QS00){for{x:=1,expect<succeeds>{x=1}}}
test(QF00){for{x:=1|2,expect<succeeds>{x=1}}}
test(QF00){for{x:=1|2,expect<succeeds>{x=2}}}
test(QS00){for{x:=1|1,expect<succeeds>{x=1}}}
test(QF00){for{array{2,4,6}[i:int]=4,expect<succeeds>{i=1}}}
test(QF00){for{array{2,4,6}[i:int]=4,expect<succeeds>{i=2}}}
test(QS00){j:=for{i where array{2,4,6}[i:int]=4},expect<succeeds>{j=array{1}}}
#!!broken by op::atom change test(F00){j:=for{i where array{2,4,6}[i:int]=4},expect<succeeds>{j=array{2}}}
test(QF00){expect<succeeds>{1=2}}

# Verify degenerate cases.
test(QS00){x:int   => expect<succeeds>{x}}
test(QS00){x:int   => expect<succeeds>{x}}
test(QS01){x:int   => expect<diverges>{x}}
test(QS02){x:int   => expect<fails   >{x}}
test(QS00){x:false => expect<succeeds>{x}}
test(QS00){x:false => expect<diverges>{x}}
test(QS02){x:false => expect<fails   >{x}}

# Allow.
test(QS00){if{expect<succeeds>{1}}}
test(QF00){if{expect<succeeds>{1=2}}}
test(QF00){if{expect<succeeds>{false[]}}}
test(QD00){a:int => if{expect<succeeds>{a=1}}}
test(QS00){a:int => if{expect<decides>{a=1}}}

# First-class static rejection. Must be a macro.
test(V02){reject{"Bad"}}
test(V03){reject{_}}
test(C00){1(2),reject{"Bad"}}
test(V02){reject{"Bad"},1(2)}
#test(V02){r:=reject{r}} #!! Paraconsistency puzzle: What ought WhenCastStep do here? Not suppress ChildFx rejects?
#then we must be rigorous

#
# Unresolved unification blocking.
#

# Basics.
test(S00){123}
test(A00){_}
test(A00){:any}
test(S00){1,2}
test(A00){_,2}
test(S00){123}
test(A00){x:=x}
test(S00){for{123}}
test(A00){f():any,f[]=3}
test(S00){for{_=(2|3)}}
test(S00){g(f():any):=f[] => g(f():=3)=3}
test(S00){g(f():any):=f[] => g(f():=3)=_}
#test(A00){for{_}}
#test(A00){for{x:any}}
#test(A00){for{x:any}=array{3}}
#test(A00){g(f():any):=f[] => g(f():=_)=3}

# Previously had strange recalculation behaviour.
test(F00){f[array{5}]=5 where f(array{5}):=3}

#
# Lennart found these problems.
#

# Runtime error: fails where disallowed.
test(S00){f(x:=(:prefix'-') where x = (:nat)):={x}; if(a:=f[3]) then Print(a) else Print("no")}
test(S00){f(x:prefix'-' where nat[x]):=x; if(f[3]) then 0 else Print[0]}

#
# Context strengths and resolves.
#

# Comparable.
test(S00){f(x:comparable):=comparable[x]}
test(S00){f(x:int):=comparable[x]}
test(QD00){f(x:rational):=int[x]}
test(D00){f(x:comparable):=int[x]}

#
# More tests.
#

# WhenCastStep.
test(A00){f(x:any):type{x=3} => f[_]}		#TODO:10.4:RANGE:TYPE???
test(A00){prefix'-'[_]}
test(A00){prefix'-'[]} # Could be U00 but missing axiom.
#LA test(M00){1{2}}
#LA test(M01){_{2}}

# Interesting effective reachability tests.
test(D00){p&q:int => f(:int):int => f[p]=3 => f[q]=5 => p=q} # Could be F00. Do we want that kind of inference?
test(F00){as:=(:int,:char) => int(as[0]) where 0=1}

# Propagation of decidability through head reification via WhenCastStep.
test(S00){2+3=5}
test(F00){2+3=6}
test(S00){x:=2,x+3=5}
test(F00){x:=2,x+3=6}
test(S00){x:int => x=2 => x+3}
test(S00){x:int => x=2 => x+3=5}
test(F00){x:int => x=2 => x+3=6}
test(QD00){x:int => allow{x=2} where x=2}        # Obvious.
test(D00){x:int => x=2 where x+3=5}             # Obvious.
test(QD00){x:int => allow{x=2} where x+3=5}      # Must hold, else it implies x=2.
test(QD00){x:int => allow{x=2} where x+3}        # Must hold too, as it's no weaker than previous.
test(QD00){x:int => allow{x=2} where int[x]=2}   # Must hold, else it implies x=2.
test(QS00){x:int => allow{x=2} where int[x]}     # Can succeed by reachability.
test(QS00){x:int => allow{x=2} where int[x]=x}   # Can succeed by reachability.
test(S00){x:any => assume{x=2} where x+3=5}
test(F00){x:any => assume{x=2} where x+3=6}
test(QD00){x:int => allow{x=2} where x+3=5}
test(QF00){x:int => allow{x=2} where x+3=6}
test(QU00){x:any => allow{x=2} where x+3=5}
test(QD00){x:int => allow{x=2} where x+3=5}
test(QF00){x:int => allow{x=2} where x+3=6}
test(S00){x:any => assume{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,6)}
test(F00){x:any => assume{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,7)}
test(QU00){x:any => allow{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,6)}
test(QF00){x:any => allow{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,7)}
test(QS00){a:int => allow{as:=array{a}=array{5}} where as[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]}
test(U00){x:int => (5,7)[x]=7} # Can narrow to D00.
test(QF00){x:int => allow{x=1} where (5,7)[x]=5}
test(QD00){x:int => allow{x=1} where (5,7)[x]}
test(QD00){x:int => allow{x=1} where (5,7)[x]=7}

#
# Vector data flow.
#

# Prior slack in vector data flow.
test(S00){int[123]}
test(S00){array{3}[0]=3}
test(A00){array{_}[0]}
test(S00){array{_}[0]=2}
test(S00){array{_}[0]=3}
test(D00){as:=array{a:int} => as[0]=3}
test(QS00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where cs[0]}
test(QS00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where cs[0] where cs[0]}
test(QS00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where cs[0]=cs[0]}
test(QD00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where as[0]=bs[0]}
test(D00){as:=array{a:int} => bs:=array{b:int} => as[0]=bs[0]}
test(QD00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where cs[0]=a}
test(QD00){as:=array{a:int} => bs:=array{b:int} => allow{cs:=as=bs} where cs[0]=as[0]}
test(D00){as:=array{a:int} => bs:=array{b:int} => as[0]=bs[0]}
test(S00){as:=array{3} => as[0]=3}
test(F00){as:=array{3} => as[0]=4}
test(F00){array{3}[0]=4}
test(F00){as:=array{3},as[0]=as[0]=4}
test(QS00){as&bs:=array{:int} => cs:=allow{as=bs} where cs[0]}
test(S00){array{2}}
test(S00){y:=2,y=2}
test(S00){y:=2 where y=2}
test(U00){a&b:any => f(x:=x)<computes>:any => f[a]=f[b]}

# Invisible dominators and projection blocking.
test(S00){as:any => as=array{_} => as[0]}
test(S00){a:any => a=(_,_) => a=(_,_)}
test(S00){as:any => as=array{_} => as[0]}
test(U00){as:any => as=array{_} => as[0]=4}
test(F00){as:=array{3} => as=array{4}}
test(F00){as:=array{3} => as[0]=4}
test(U00){a:any => a=(_,_) => a=(4,_)}
test(U00){as:any => as=array{_} => as[0]=4}
test(S00){a:=(3,7) => a=(_,_)}
test(S00){a:any => a=(_,_) => a=(_,_)}
test(F00){c:any => c=array{8} => c=array{9}}
test(U00){c:any => c=array{9}}
test(F00){c:any => c=3 => c=4}
test(F00){c:any => c=array{8} => c=array{9}}
test(F00){c:any => assume{c=array{8}} where c=array{9}}
test(F00){c:any => c=array{9} where assume{c=array{8}}}
test(F00){c:any => {f()<resolves>:=c=array{8},f[],c=array{9}}}
test(S00){a:any => a=(3,7) => a}
test(S00){a:any => a=(3,7) => a[0]}
test(F00){a:any => a=(3,7) => a[0]=4}
test(S00){a:any => a=(3,7) => a=(_,_)}
test(S00){a:any => a=as where as:=(a0:=3,a1:=7) => a=(b0:=_,b1:=_)}
test(F00){f(:any)<computes>:any => f(2)=(_,_) => f[2]=(_,_,_)}
test(QS00){as:=array{a:=3 } => bs:=as where allow{bs[0]=3}}
test(QD00){as:=array{a:int} => bs:=as where allow{bs[0]=3}}
test(QU00){as:=array{a:any} => bs:=as where allow{bs[0]=3}}
test(S00){a:any => a=array{_} => a[0]}
test(U00){a:any => a=array{_} => a[0]=2}
test(S00){a:any => a=array{3} => a[0]=3}
test(F00){a:any => a=array{3} => a[0]=4}
test(S00){a:any => a=array{3} => a=array{_}}
test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=(_,_)}
test(S00){f(:any)<computes>:any => f[2]=(_,_) => f[2]=(_,_)}
test(S00){f(:any)<computes>:any => f(2)=(_,_) => f[2]=(_,_)}

# Eta Conversion.
test(F00){as:=array{a:=8} => c:=as[0]=9}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => {A:=as[0],B:=bs[0],C:=A=B}}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => cs:=as=bs}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => c:=as[0]=bs[0]}
test(D00){as:=array{a:int} => bs:=array{b:int} => {A:=as[0],B:=bs[0],C:=A=B}}
test(D00){as:=array{a:int} => bs:=array{b:int} => {A:=as[0],B:=bs[0],C:=A=B}}
#
test(QS00){a&b:int => allow{c:=a=b} where a=a}
test(QS00){a&b:int => allow{c:=a=b} where b=b}
test(QS00){a&b:int => allow{c:=a=b} where c=c}
test(QD00){a&b:int => allow{c:=a=b} where a=c}
test(QD00){a&b:int => allow{c:=a=b} where b=c}
test(QD00){a&b:int => allow{c:=a=b} where a=b}
#
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where as[0]}
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where bs[0]}
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where cs[0]}
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where as[0]=as[0]}
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where bs[0]=bs[0]}
test(QS00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where cs[0]=cs[0]}
test(QD00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where as[0]=cs[0]}
test(QD00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where bs[0]=cs[0]}
test(QD00){as:=array{:int} => bs:=array{:int} => allow{cs:=as=bs} where as[0]=bs[0]}
#
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where a}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where b}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where bs[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where cs[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]=as[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where bs[0]=bs[0]}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where cs[0]=cs[0]}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]=cs[0]}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where bs[0]=cs[0]}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as[0]=bs[0]}

# Getting to the heart of directed equality and dominators.
test(QS00){a&b:int => allow{p:=(a,b),p=(b,a),expect<succeeds>{p[0]}}}
test(QS00){a&b:int => allow{c:=a=b} where c}
test(QS00){a&b:int => allow{c:=a=b} where a=a}
test(QD00){a&b:int => allow{c:=a=b} where c=a}
test(QS00){a&b:int => allow{c:=a=b} where int[a]}
test(QD00){a&b:int => allow{c:=a=b} where int[c]}
test(QS00){a:=array{:int} => b:=array{:int} => allow{c:=a=b} where a=a}
test(QS00){a:=array{:int} => b:=array{:int} => allow{c:=a=b} where c=c}
test(QD00){a:=array{:int} => b:=array{:int} => allow{c:=a=b} where c=a}
test(QS00){a:=array{:int} => b:=array{:int} => allow{c:=a=b} where a=array{:int}}
test(QD00){a:=array{:int} => b:=array{:int} => allow{c:=a=b} where c=array{:int}}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as=as}
test(QS00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where cs=cs}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as=cs}
test(QD00){a&b:int => allow{x:=a,y:=b,z:=x=y} where int[x]} # Must be D00, like it or not.
test(QD00){a&b:int => allow{x:=a,y:=b,z:=x=y} where int[z]}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where as=array{:int}}
test(QD00){a&b:int => allow{as:=array{a},bs:=array{b},cs:=as=bs} where cs=array{:int}} # Slack.

# Figure out if tests can expose reduced dominator granularity in arrays.
test(QS00){a&b:int => allow{x:=a=b} where x}
test(QS00){as:=(:int,:int) => bs:=(:int,:int) => xs:=allow{as=bs} where xs}
test(QS00){as:=(:int,:int) => bs:=(:int,:int) => xs:=allow{as=bs} where xs[0]}

#
# Array data flow.
#

# Functions and parameters not seen by direct visibility.
test(C00){f:any => f[_]=3}
test(S00){f:any => f=int => f[_]=3}
test(F00){f:any => f=int => f[_]='x'}		#TODO:CHECK-CHAR?
test(S00){f:any => f[_]=3 where assume{f=int}}
test(S00){p:any => p=2 => p+3=5}
test(F00){p:any => p=2 => p+3=6}
test(A00){x:any,x+3=5}
test(A00){x:any,operator'+'[x,3]=5}
test(A00){x:any,assume{x=2},x+3=5}		#TODO:10.4:ASSUME
test(F00){x:any,assume{x=2},x+3=6}
test(A00){x:any,int[x]}
test(A00){x:any,assume{x=2},int[x]}		#TODO:10.4:ASSUME
test(S00){x:any,assume{x=2},x=2}
test(R20){x:any,assume{x=2},x=3}
test(A00){x:any,assume{x=2},x+3=5}		#TODO:10.4:ASSUME
test(S00){x:any => x=2 => x=2}
test(F00){x:any => x=2 => x=3}
test(S00){x:any => x=2 => x+3=5}
test(F00){x:any => x=3 => x+3=5}
test(S00){x:any => x+3=5 where assume{x=2}}
test(F00){x:any => x+3=5 where assume{x=3}}
test(S00){f:any => f[_]='x' where assume{f=char}}
test(F00){f:any => f[_]='x' where assume{f=int}}
test(A00){x:any,assume{x=2},x+3=5}	# NOTE: duplicate with L2071 above	TODO:10.4:ASSUME
test(F00){x:any,assume{x=2},x+3=6}

# A call that doesn't succeed should block function equality. f[a]=b <=> f=function(a){b}.
test(QU00){a:any   => f(x:nat):any => g:=f where allow{g[a]}}
test(QD00){a:int   => f(x:nat):any => g:=f where allow{g[a]}}
test(QS00){a:nat   => f(x:nat):any => g:=f where allow{g[a]}}
test(QU00){a:any   => f(x:nat):int => g:=f where allow{g[a]=3}}
test(QD00){a:int   => f(x:nat):int => g:=f where allow{g[a]=3}}
test(QD00){a:nat   => f(x:nat):int => g:=f where allow{g[a]=3}}
test(QU00){a:any   => {f(x:nat):=3,g:=f where allow{g[a]}}}
test(QD00){a:int   => {f(x:nat):=3,g:=f where allow{g[a]}}}
test(QS00){a:nat   => {f(x:nat):=3,g:=f where allow{g[a]}}}
test(QU00){a&b:any => {f(x:nat):=3,g:=f where allow{g[a]=b}}}
test(QD00){a&b:int => {f(x:nat):=3,g:=f where allow{g[a]=b}}}
test(QD00){a&b:int => {f(x:nat):=3,g:=f where allow{g[a]=b}}}

# Prior unsoundness.
test(F00){as:=array{2},bs:=array{3},as[0]=bs[0]}
test(F00){as:=array{2},bs:=array{3},as=bs}
test(F00){as:=array{2} where bs:=array{3} where as=bs where as[0]=bs[0]}
test(F00){as:=array{2},bs:=array{3},as=bs,as[0]=bs[0]}
test(D00){a&b:int => {as:=array{a},bs:=array{b},cs:=as=bs,as[0]=bs[0]}}
test(F00){array{2}=array{2}=array{3}=array{3}}

# Strange breakage from Strength equated-initial dominators flow, not fixed.
test(D00){a:int => c:any => c=(a,3) => c=(3,a)}
test(D00){a:int => c:any => assume{c=(a,3)} where c=(3,a)}
test(D00){a:int => c:any => f():type{c=3} => {f[],c=a}}
test(S00){c:any => assume{c=array{3} where c=array{4}}}
test(F00){c:any => f():=c=array{3} => {f[],c=array{4}}}
test(D00){a:int => c:any => f():type{c=(a,3)} => {f[],c=(3,a)}}
test(F00){c:any => f():=c=array{8} => {f[],c=array{9}}}
test(F00){c:any => f(7):=c=array{8} => {f[7],c=array{9}}}
test(F00){c:any => f(7):=c=array{8} => f[7] where c=array{9}}
test(F00){c:any => f(7)<computes>:=c=array{8} => {f[7],c=array{9}}}
test(F00){c:any => f(7)<computes>:=c=array{8} => f[7] where c=array{9}}
test(D00){a&b:int => c:any => f():type{c=(a,b)} => {f[],c=(b,a)}}
test(D00){a&b:int => c:any => f():type{c=(a,b)} => {c=(b,a),f[]}}
test(D00){a&b:int => c:any => f():type{c=array{a}} => {c=array{b},f[]}}

# Checking vector flow.
test(QU00){as:=array{_} => bs:=as where allow{bs[0]=3}}
test(QF00){as:=array{a:=4} => bs:=as where allow{bs[0]=3}}

# Strange prior breakage.
test(QD00){a&b:int => f(:int)<computes>:int => allow{a=b} where f[a]=f[b]}
test(QD00){a&b:int => f(:int)<computes>:int => allow{a=b} where f(a)=f(b)}
test(QD00){a:int => f(:int)<computes>:int => allow{a=3} where f[a]=f[3]}
test(QD00){a:int => f(:int)<computes>:int => allow{a=3} where if{f[a]}=f[3]}

# Assumption monotonicity.
test(QS00){a:int => if{x:=a=2,expect<succeeds>{x}}}
test(S00){a:int => if{x:=a=2,assume{a=1}}}

# Simplest scalar shootdown counterexamples.
test(D00){f(x:int):int => f[7]=7}

# Vector dominators on function.
test(S00){as:=array{a:any} => {bs:=as,bs[0]}}
test(QU00){as:=array{a:any} => {bs:=as,allow{bs[0]=3}}}
test(S00){f(3):=4 => f[3]}
test(S00){a:comparable => a=a}
test(U00){a&b:any => a=b}
test(D00){a&b:comparable => a=b}
test(U00){f(x:any):any => f[3]=f[3]}		#TODO:FUN-OUT-EQ
test(D00){f(x:any):comparable => f[3]=4}
test(D00){f(x:any):comparable => f[3]=f[4]}
test(D00){f(x:any):comparable => f[3]=f[3]}	#TODO:FUN-OUT-EQ
test(QS00){f(x:any):any => allow{f[3]=4}}
test(QU00){f(x:any):any => g:=f where allow{g[3]=4}}
test(QD00){f(x:any):int => g:=f where allow{g[3]=4}}
test(QD00){f(x:comparable):comparable => g:=f where allow{g[3]=4}}
test(S00){comparable[123]}
test(S00){comparable[123]}
test(S00){a:any => {f(x:any):=a,f[3]}}
test(QU00){a:any => {f(x:any)<computes>:=a,g:=f,allow{g[3]=4}}}
test(QD00){a:comparable => {f(x:comparable)<computes>:=a,g:=f,allow{g[3]=4}}}
test(D00){a:comparable => bs:=array{:comparable} => a=bs[0]}
test(D00){a&b:comparable => f[]=a where f():=b}
test(D00){a&b:int => a=b}
test(D00){f(x:any):int => f[3]=4}
test(D00){a:comparable => f():comparable => f[]=a}

# Unseen functions.
test(S00){f(x:int):=x, a:any => a=f => a[_]=3}			#TODO:SKIP:FUN-EQ
test(F00){f(x:int):=x, a:any => a=f => a[_]='3'}		#TODO:SKIP:FUN-EQ

# Function breakage resulting from visibility narrowing, which NEWFLOW doesn't patch for functions.
test(U00){a&b:any => f(x:=x)<computes>:any => f[a]=f[b]}
test(S00){a:int => b:int => f():type{a=b} => {f[],a=b}}
test(S00){a&b:int => f():type{a=b} => {f[],a=b}}

# Contingency timing sensitivity.
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[a,b],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[a,b],x=y}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],x=y}}
#MakeIdentityOp revealed housekeeping bug test(D00){a&b:int => f(x&y:int):type{x=y} => if{x:=a,y:=b,f[x,y],x=y,expect<succeeds>{x=y}}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,a=b,f[x,y]}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,x=y,f[x,y]}}
#MakeIdentityOp revealed housekeeping bug test(D00){a&b:int => f(x&y:int):type{x=y} => if{x:=a,y:=b,x=y,f[x,y],expect<succeeds>{x=y}}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],a=b}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}
test(D00){a&b:int => f(x&y:nat):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}
test(U00){a&b:any => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}

# Arrays.
test(S00){array{1,2}=array{1,2}}
test(F00){array{1,1}=array{1,2}}
test(F00){array{1,2}=array{1,3}}
test(F00){array{1,1}=array{2,1}}
test(F00){array{2,1}=array{3,1}}
test(F00){array{1,2}=array{2,1}}
test(F00){array{1,2}=array{3,4}}
test(S00){as:=array{1} => as=array{1}}
test(F00){as:=array{1} => as=array{2}}
test(S00){as:=array{1} => bs:=array{1} => as=bs}
test(F00){as:=array{1} => bs:=array{2} => as=bs}

# LocalFunctions will fix this.
test(S00){(f(:int)<computes>:int,f[3]=4)=>f[3]=4}
test(S00){(f(:int)<computes>:int,f[3]=4)=>f(3)=4}
test(F00){(f(:int)<computes>:int,f[3]=4)=>f[3]=5}
test(F00){(f(:int)<computes>:int,f[3]=4)=>f(3)=5}

# Attaching call info to functions directly.
test(U00){a&b:any => f(x:=x)<computes>:any => f[a]=f[b]}
test(U00){a:=a => b:=b => f(x:=x)<computes>:=x => f[a]=f[b]}
test(D00){a&b:int => f(x:int)<computes>:=x => f[a]=f[b]}

# Sound array dominators.
test(QD00){a:int=>allow{cs:=Concatenate[array{array{a}}] where ds:=cs where ds[0]=8} where cs=ds}
test(QD00){a:int=>allow{cs:=Concatenate[array{array{a}}] where (ds:=cs)[0]=8} where cs=ds}

# Investigate. Should work with no subtle considerations.
test(S00){f(a->b:prefix'-'):=(a,b) => f[3]=(3,-3)}

# Calling functions.
test(A00){:int}
test(S00){f(1):=2 => f[1]}
test(F00){f(1):=2 => f[3]}
test(P00){f(1):=2 => f(3)}
test(S00){f(1):=2 => f[1]=2}
test(F00){f(1):=2 => f[1]=3}
test(S00){f(x:int):=x => f[7]}
test(S00){f(x:int):=x => f[7]=7}
test(S00){f(x:int):int => f[7]}
test(D00){f(x:int):int => f[7]=7}
test(F00){f(x:=7):=x => f[9]}

# Probing context_domain strength dependence on matching function&parameter strength.
test(QS00){a&b:int => f(:nat)<computes>:int => f[a] => allow{a=b} where f[a]}
test(QS00){a&b:int => f(:nat):int => f[a] => allow{a=b} where f[a]}
test(QD00){a&b:int => f(:nat)<computes>:int => f[a] => allow{a=b} where f[b]}
test(QD00){a&b:int => f(:nat):int => f[a] => allow{a=b} where f[b]}
#test(D00){a:int => (f&g)(:nat):int => f[a] => allow{f=g} where g[a]} # Pending call-multiple-functions.

# Consider interaction of dominators, virtual elements, and functions as follows.
test(QS00){a&b:int        => allow{c:=a=b} where int[a]}
test(QS00){a&b:int        => allow{c:=a=b} where int[b]}
test(QD00){a&b:int        => allow{c:=a=b} where int[c]} # Slack.
test(QD00){a:nat => b:int => allow{c:=a=b} where nat[c]} # Not slack.
test(QD00){a:nat => b:int => f(x:nat):=x => allow{c:=a=b} where f[a]=f[b]}

# Representative tests that were previously broken by array matching.
test(S00){for(a,1|2){-a},a:=3}
test(S00){for(x:=a|b,y:=1|2){x+y}=array{11,12,21,22},a:=10,b:=20}
#!!test(S00){for(x:xs,y:=1|2){x+y}=array{11,12,21,22},xs:=(10,20)}
test(D00){a&b:int => f(x:int)<computes>:=x => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => f[a]=f[b]}
test(S00){a:int => f(x:int):type{x=3} => f[a]}
test(QD00){a:int => {x:=int[a],allow{x=3}}}
test(S00){a&b:int => f(x&y:int):type{x=y} => f[a,b]}
test(S00){a&b:int => f(x&y:int):type{x=y} => {a=b,f[a,b]}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {f[a,b],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],a=b}}
test(QD00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{a=c}}}
test(QS00){a:int => b:any => f(x:int):=int[x] => expect<succeeds>{f[a]}}
test(QU00){a:int => b:any => f(x:int):=int[x] => expect<succeeds>{f[b]}}
test(QS00){a:int => b:any => f(x:int):=int[x] => allow{a=b} where f[a]}
test(QU00){a:int => b:any => f(x:int):=int[x] => allow{a=b} where f[b]}
test(QD00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=a}}} # Consider.

# Dominators and escalation avoidance.
test(QS00){a&b:int => if{x:=a=b,expect<succeeds>{x}}}
test(QD00){a&b:int => if{x:=a=b,expect<succeeds>{int[x]}}}
test(QD00){a&b:int => f(a=b):=a=b => if{x:=a=b,expect<succeeds>{f[x]}}}
test(D00){a&b:int => f(a=b):=a=b => {x:=a=b,f[x]}}
test(QD00){a&b:int => f(x:int):=int[x] => if{x:=a=b,expect<succeeds>{f[x]}}}

# Known.
test(QS00){known[123]}
test(QS00){known[3,5]}
#!!!!test(U00){x:int=>known[x]}
#test(U00){x:int=>known[3,x]}
test(QS00){x:=4=>known[3,x]}
test(QA00){known[x:=x]}

# Domains.
test(QS00){x&y:int => f(:int)<computes>:int => f[x] => {a:=allow{x=y},f[x]}}
test(QD00){x&y:int => f(:int)<computes>:int => f[x] => {a:=allow{x=y},f[a]}}
test(QS00){x&y:int => f(:int):int => f[x] => {a:=allow{x=y},f[x]}}
test(QD00){x&y:int => f(:int):int => f[x] => {a:=allow{x=y},f[a]}}

# Concatenation tests.
test(S00){array{}=array{}}
test(S00){for{}=for{}}
test(S00){array{}=Concatenate[]}
test(S00){Concatenate[array{},array{}]}
test(S00){Concatenate[array{},array{7}]}
test(S00){Concatenate[array{2},array{3}]=array{2,3}}
test(F00){Concatenate[array{2},array{3}]=array{8,9}}
test(S00){Concatenate[array{},array{3}]=array{3}}
test(F00){Concatenate[array{},array{3}]=array{8}}
test(F00){Concatenate[array{},array{7}]=array{8,9}}
test(F00){as:=array{},bs:=array{3},Concatenate[as,bs]=array{8}}
test(F00){array{2}=array{3}}
test(F00){for{2}=array{3}}
test(F00){for{2=2}=array{3}}
test(S00){for{1=2}=array{}}
test(S00){for{1|2}=array{1,2}}
test(S00){for{1|2|3}=array{1,2,3}}
test(S00){for{1|(2=3)|3}=array{1,3}}
test(S00){Concatenate[array{3},array{3}]=array{3,3}}
test(F00){Concatenate[array{3},array{3}]=array{3,4}}
test(F00){Concatenate[array{3},array{3}]=array{4,3}}
test(F00){Concatenate[array{3},array{3}]=array{3}}
test(F00){Concatenate[array{3},array{3}]=array{4}}
test(S00){Concatenate[Concatenate[array{},array{3}],array{4}]=array{3,4}}
test(F00){Concatenate[Concatenate[array{},array{3}],array{4}]=array{3,3}}
test(F00){Concatenate[Concatenate[array{},array{3}],array{4}]=array{4,4}}
test(S00){as:=array{a:=3},cs:=Concatenate[array{},as] where A:=a,cs}
test(S00){Concatenate("ABC","def")="ABCdef"}

# Past ForVisible breakage.
test(A00){array{a:=a,b:=b}[1]=9}
test(D00){f():int => g():char => f[]=g[]}
test(D00){f()<computes>:int => g()<computes>:int => f[]=g[]}
test(D00){f()<computes>:int => g():int => f[]=g[]}
test(D00){f():int => g()<computes>:int => f[]=g[]}
test(D00){f():int => g():int => f[]=g[]}
test(D00){f(x:int):int => f[3]=f[3]}		#TODO:FUN-OUT-EQ
test(D00){f(x:int)<computes>:int => f[3]=f[4]}

# Reification.
test(D00){x:int=>{x=3,x+1=4}}
test(F00){x:int=>{x=3,x+1=5}}
test(F00){f(x:int):=array{x,2}=array{3,x}}

# Was heinously slow due to quadratic constraint_call pair handling, now fast!
test(S00):
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7

# Combinatorics.
#!!obscure Concatenate breakage test(S00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=2 or n=3}}
#!!test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=2}}
test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=1 or n=2 or n=3}}
test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=2 or n=3}}
test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=3}}

# Unification as macro taking inputs.
test(S00){f(x:prefix'-'):=x,f(3)=-3}
test(S00){f(x:=(:prefix'-')=(:prefix'-')):=x,f(3)=-3}
test(P00){f(x:=(:prefix'-')=(:int)):=x,f(3)}
test(S00){f(x:=(:prefix'-')=(:int)):=x,f(0)}

# Broke with ArrayMacro input improvements, now fixed.
test(D00){a&b:int => f(x&y:int):=x=y => g(a):int => h(b):int => f[g[1],h[2]]}
test(F00){a&b&c:int => f(a,x&y:int):type{x=y} => g(b):int => h(c):int => f[3,g[1],h[2]]}

# Arrays with inputs.
test(S00){f(a:prefix'-',b:int):=(b,a),f[3,7]=(7,-3)}
test(F00){f(a:prefix'-',b:int):=(b,a),f[3,7]=(7,3)}

# Can we construct a test to show that ForVisibleHelper use of Global flag is essential?
test(D00){as:=(:int,:int) => bs:=as => bs=(3,5)}
test(S00){as:=(:int,:int) => bs:=as => bs=(:int,:int)}
test(U00){as:=(:int,:int) => bs:=(:int,:int) => cs:=as => ds:=bs => o:any => {es:=cs=ds,es=(o,o)}}

# Fails-cancellation.
#test(S00){a:int => if(a=1){a=1}else{not a=1}} # works withh fails-cancellation
test(D00){a:int => if(a=1){a=1}else{a=1}}  # breaks withh fails-cancellation

# Is this the NewExplorations timing problem?
test(F00){x:int => f():=x=1|2 => {f[],first{x=1}}}
test(F00){x:int => f():=x=1|2 => {first{x=1},f[]}}

# Works, but why is performance not greatly improved by suspending DecidesFork?
test(S00){as:=(:int,:int,:int,:int,:int) => for{a where i->a:as,a=i}}

# Nonmonotonicity with functions.
test(F00){function(g(1):=2){g}[function(0){2}][1]}
test(F00){function(g(1):=2){g}[function(0){2}][1]}

# Iteration-stop coverage.
test(S00){a:int=>{b:int=>if(a=2){c=3}else{c=3},assume{a=2}},c:=3}
test(S00){a:int=>{b:int=>if(a=2){c=3}else{c=3},assume{a=1}},c:=3}
test(S00){a:any=>{b:int=>if(a=2){c=3}else{c=4},assume{a=2}},c:=3}
test(S00){a:=2=>{b:int=>if(a=2){c=3}else{c=4}},c:=3}
test(S00){a:=1=>{b:int=>if(a=2){c=4}else{c=3}},c:=3}
test(S00){a:int=>{b:int=>if(a=2){c=3}else{c=4},assume{a=2}},c:=3}
test(S00){a:int=>{b:int=>if(a=2){c=4}else{c=3},assume{a=1}},c:=3}
test(F00){0=>c=4,c:=3}
test(F00){a:=2=>{b:int=>if(a=2){c=4}else{c=3}},c:=3}
test(F00){a:=1=>{b:int=>if(a=2){c=3}else{c=4}},c:=3}
test(F00){a:int=>{b:int=>if(a=2){c=4}else{c=3},assume{a=2}},c:=3}
test(F00){a:int=>{b:int=>if(a=2){c=3}else{c=4},assume{a=1}},c:=3}

# Adding assumptions to ExploreDecides.
test(F00){x:=1|2 => first{x=1}}
test(S00){x:int => f():type{x=1} => {first{x=1},f[]}}
test(F00){x:int => f():type{x=2} => {first{x=1},f[]}}
test(F00){x:int => f():type{x=2|1} => {first{x=1},f[]}}
test(F00){x:int => f():type{x=2|3} => {first{x=1},f[]}}
test(S00){a:int=>if(a=2) then assume{3|4} else assume{5|6}} # Works, but shows Refork check is too early.
#CHILDFX ABANDON test(S00){x:int => f():=1|2 => if(x=f()){1}else{2}}
test(F00){x:int => f():=x=1|2 => {first{x=1},f[]}}

# Nested comprehensions.
test(S00){W->X:=for(0|1){2} where W}
test(S00){for(r:=for(0..3) do 'X'|'Y') do Print(r)}
test(S00){for{for(){0|1}}}
#!!ReduceIdentifierStep simple timing change trigger new bug test(S00){for{for(x:=2|1){0|x}}=((0,0),(0,1),(2,0),(2,1))}

# Condition transitions must be monotonic.
test(S00){a:any => b:int => {if{b=3}}}
test(U00){a:any => b:int => {x:=b,if{x=3},x=a}}
test(U00){a:any => b:int => {x:=a,if{x=3},x=b}}
test(QS00){a:any => b:int => {allow{b=a,if{b=3}}}}
test(QS00){a:any => b:int => {allow{if{b=3},b=a}}}

# Interesting conditions.
test(D00){a:int => if(a=1){a=1}else{a=1}}
test(D00){a:int => if(a=1){a=1}else{a=2}}
test(F00){a:int => if(a=1){a=1}else{not a=2}}
#test(S00){a:int => if(a=1){a=1}else{not a=1}} # failure cancellation problem?

# New instances of iteration unsoundness.
#test(U00){f(x:int):int => {first{int[x]},x:=f[x]}}
test(F00){a:int => f(:nat):int => {x:=if(f[a]){1}else{2},nat[a],x=1}}

# Functions with conditions.
#!!ReduceIdentifierStep simple timing change trigger new bug test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,4]}
#!!test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,3]}
#!!test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,3]}
#!!test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,4]}
#!!test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,:int]}
#!!test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,:int]}
#!!test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => if{f[a,3]}}
#!!test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => f[a,3]}
#!!test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => f[a,4]}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y) => f[0,3]}

# New breakage.
#test(S00){f(1|2):int => for{f[x:int]=x}}

# Iteration and suspensions.
test(S00){for{x=1|2,x:int}}
test(S00){for{x:int,x=1|2}}
test(S00){for{array{2,3}[i:nat]}}
test(S00){for{x:int,x=1|2}}
test(S00){a:int => for{int[a],b:=1|2,b=3}}
test(S00){a:any => for{int[a],b:=1|2,b=3}}
test(S00){for{let(x:=1|2) {x+1}}=(2,3)}
test(S00){for{x+1 where x:=1|2}=(2,3)}

# Unification and flexibility with nesting.
test(S00){as:=(:int,:int) => as=as}
test(S00){as:=(:int,:int) => as=(as(0),as(1))}
test(D00){as:=(:int,:int) => as=(as(0),as(0))}
test(F00){as:=(:int,3) => as=(x:=x,4)}
test(S00){a:=(2,3),a=(x:any,y:any)}
test(S00){a:=(2,3),a=(x:=x,y:any)}
test(S00){a:=(2,3),a=(x:any,y:=y)}
test(S00){a:=(2,3),a=(x:=x,y:=y)}
test(S00){a:=2,a=(x:=x)}
test(S00){a:=2=>a=(x:=x)}
test(S00){as:=array{2} => as=array{x:=2}}
test(S00){as:=array{2} => as=array{x:any}}
test(S00){as:=array{2} => as(0)=(x:=x)}
test(S00){as:=(:int,:int) => as=(x:=x,y:=y)}
test(S00){as:=(2,3) => as=(x:=x,y:=y)}
test(S00){as:=array{2} => as=array{x:=x}}

# Timing change to syntax.call broke some surprising stuff:
test(S00){f:=function(x:=int[x]){x},f(1)}
test(F00){function(x:=-3){x}[3]}
test(S00){if{f(if{c=1}):=8},c:=7}

# Non-top-level tests.
#LA test(S00){test(S00){123}}
#LA test(F00){test(S00){2=3}}
#LA test(X00){test(S00){Print(3)}=>3}

# Condition freedom.
test(S00){a&b&c:int => {x:=a,if(x=b){x=a,x=b}}}
test(S00){a:int => if(nat[a]) then nat[a]}
test(S00){a:int => {x:=a,if(nat[x]) then nat[x]}}
test(D00){a&b&c:int => {x:=a,if(x=b){x=a,x=b,x=c}}}
test(S00){a&b:int => {x:=a,if(x=a){1}else{2}}}      # succeeds
test(S00){a&b:int => {if(x=a){1}else{2},x:=a}}      # waits for decidable, succeeds
test(S00){a&b:int => {x:=b,if(x=a){a=b}else{2}}}    # decidable; assumes and succeeds
test(S00){a&b:int => {if(x=a){a=b}else{2},x:=b}}    # waits for decidable; assumes and succeeds; succeeds
test(D00){a&b:int => {x:=a,if(x=a){1}else{2},x=b}}  # succeeds, may fail
test(D00){a&b:int => {x:=b,if(x=a){1}else{2},x=a}}  # decidable; assumes and succeeds; may fail

#
# To categorize.
#

# Conditions with choice.
test(S00){if(a:=1|2){a=1}}
test(F00){if(a:=1|2){a=2}}
test(F00){x:int => if(x=1|2){x=1}}
test(F00){x:int => if(x=1|2){x=2}}
test(F00){x:int => if(x=1|2){x=3}}
test(S00){x:int => if(x=1|2){if{x=1}else{x=2}}}
test(S00){x:int => if(x=1|2){x=1 or x=2}}
test(S00){x:int => if(x=1|2){x=1 or x=3 or x=2}}
test(F00){x:int => if(x=1|3|2){x=1 or x=2}}

# Context promotion recognition.
test(S00){x:int => f():type{x=2} => {first{x=2},f[]}}
test(S00){x:int => f():type{x=2} => {first{x=2},if{f[]}}}

# Conditional stops.
test(S00){x:int => f():type{x=0} => {if(x=0){1}else if(x=1){2}else{3}=1,f[]}}
test(F00){x:int => f():type{x=1} => {if(x=0){1}else if(x=1){2}else{3}=1,f[]}}
test(F00){x:int => f():type{x=2} => {if(x=0){1}else if(x=1){2}else{3}=9,f[]}}
test(F00){x:int => f():type{x=0} => {if(x=0){1}else if(x=1){2}else{3}=2,f[]}}
test(S00){x:int => f():type{x=1} => {if(x=0){1}else if(x=1){2}else{3}=2,f[]}}
test(F00){x:int => f():type{x=2} => {if(x=0){1}else if(x=1){2}else{3}=2,f[]}}
test(F00){x:int => f():type{x=0} => {if(x=0){1}else if(x=1){2}else{3}=3,f[]}}
test(F00){x:int => f():type{x=1} => {if(x=0){1}else if(x=1){2}else{3}=3,f[]}}
test(S00){x:int => f():type{x=2} => {if(x=0){1}else if(x=1){2}else{3}=3,f[]}}

# Function verification held <abstracts> until verified to hide lost notification problem.
test(D00){a:int => f():=a=3}
test(D00){a&b:int => if{a=3,f():=b=4}}
test(D00){x:int=>if{x=3,f():=x=3}} # Was S00 before UncommittableNegotiable.
test(S00){f(if{c=1}):=8,c:=7}
test(U00){a:any=>{f(if{c=1}):=8,c:=a}}	#TODO:10.4:WHY???
test(S00){if{f(if{c=1}):=8},c:=7,c=7}
test(S00){if{f(if{c=1}):=8},c:=7}
test(S00){g(9):=8,f(9 where for{x:=1}):=8,f[9],-1}

# Function range verification failure.
test(F00){f():false}
test(S00){if(f():=1){1}else{2}=1}
test(F00){if(f():false){1}else{2}}

# Function specifier allowance.
test(S00){f(0)<invariant>:=1}
test(S02){f(0)<covariant>:=1}
test(S02){f(0)<contravariant>:=1}
test(S00){f(0)<converges>:=1}
test(S00){f(0)<computes>:=1}
test(QS00){assume<succeeds>{1}}
test(QS02){assume<covariant>{1}} # Bad name resolution error. Assume should resolve!
test(QS00){expect<succeeds>{1}}

# Contingency irrelevance.
test(S00){assume{1}=assume{1}}
test(QS00){1=assume<decides>{1}}
test(QS00){1=assume<abstracts>{1}}
test(F00){assume{1}=assume{2}}
test(F00){assume{1}=2}
test(S00){a&b:int => assume{a}=assume{a}}
test(S00){a&b:int => assume{a}=a}
test(D00){a&b:int => assume{a}=assume{b}}
test(D00){a&b:int => assume{a}=b}
test(U00){a&b:any => assume{a}=assume{b}}
test(U00){a&b:any => assume{a}=b}

# Contingency basics.
test(D00){x:=assume{:int},y:=assume{:int},x=y}
test(R00){x:=assume{:int},int[x]}
test(D00){x:=assume{:int},x=3}
test(D00){x:=assume{:int},nat[x]}
test(D00){x:int => nat[x]}
test(U00){x:=assume{:any},y:=assume{:int},x=y}		#TODO:10.4:ASSUME
test(U00){x:=assume{:int},y:=assume{:any},x=y}		#TODO:10.4:ASSUME
test(U00){x:=assume{:any},x=3}						#TODO:10.4:ASSUME

# Consider information parity and soundness.
test(S00){a:int => b:any => f(:int):type{a=b} => {f[a],a=b}}
test(S00){a:int => b:any => f(:int):type{a=b} => {a=b,f[a]}}
test(S00){a:int => b:any => f(a   ):type{a=b} => {f[a],a=b}}
test(S00){a:int => b:any => f(a   ):type{a=b} => {a=b,f[a]}}
test(U00){a:int => b:any => f(:int):type{a=b} => f[a=b]}
test(U00){a:int => b:any => f(a):type{a=b} => f[a=b]}
test(S00){b:any => f(x:any):type{int[x]} => {f[b],int[b]}}
test(U00){b:any => f(x:int):=int[x] => f[b]}

# Runtime abstracts.
test(S00){for(x:int,y:=0|1,x=y) do Print(x)}

# Fixed notification problem.
test(S00){rs:=for(){r:=a},a:=3}

# Looks like array matching causes exploration notification timing issue.
test(QD00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=a}}}

# Strange verifier spines.
test(R00){g:=assume<decides>{f(:int):int},g[3]} # Assumes by convention.

# Conditions and assumptions.
test(D00){a:int => {a=3,if{a=3}}}
test(D00){a:int => {if{a=3},a=3}}
test(S00){a&b:int => f():type{a=b} => {if{a=b} else f[]}}
#!!!!test(S00){a&b:int => f():type{a=b} => {if{a=b,f[]} else f[],a=b}}
#!!!!test(S00){a&b:int => f():type{a=b} => {if{f[],a=b} else f[],a=b}}
test(D00){a&b:int => f():type{a=b} => {if{a=b} else f[],a=b}} # Could be S00
test(D00){a&b:int => f():type{a=b} => {a=b,if{a=b} else f[]}} # Could be S00

# operator'or'.
test(S00){1 or 2}
test(S00){(1 or 2)=1}
test(F00){(1 or 2)=2}
test(S00){:false or 2}
test(F00){(:false or 2)=1}
test(S00){(:false or 2)=2}
test(F00){(:false)or(:false)}

# Find.
test(S00){first{3}=3}
test(F00){first{3=4}}
test(F00){first{3}=4}
test(S00){first(x:=1|2){x+1}=2}
test(F00){first(x:=1|2){x+1}=3}

# Conditions and reachability, synchronous and asynchronous.
test(S00){if(1){2}else{3}}
test(S00){if(1=2){2}else{3}}
test(S00){if(1){2}else{3=4}}
test(S00){if(1=2){2=3}else{3}}
test(S00){if(1){2}else{3}=2}
test(F00){if(1){2}else{3}=3}
test(S00){if(1=2){2}else{3}=3}
test(F00){if(1=2){2}else{3}=2}
test(S00){if(x=1){2}else{3}=2,x:=1}
test(F00){if(x=1){2}else{3}=3,x:=1}
test(S00){if(x=2){2}else{3}=3,x:=1}
test(F00){if(x=2){2}else{3}=2,x:=1}
test(D00){a&b    :int => {if{a=b}     else {},a=b}}
test(D00){a&b&c&d:int => {if{a=b,c=d} else {},a=b}}
test(D00){a&b&c&d:int => {if{a=b}     else {},a=b,c=d}}

# Unifications and tests.
test(D00){a&b:int => {if{a=b},a=b}}
test(D00){a&b:int => {a=b,if{a=b}}}

# Decidable condition confluence.
test(S00){x  :int => f():type{x=1} => {    if(x=1){}                }}
test(S00){x  :int => f():type{x=1} => {f[],if(x=1){}                }}
test(S00){x  :int => f():type{x=1} => {    if(x=1){}            ,f[]}}
test(F00){x  :int => f():type{x=1} => {    if(x=1){}else{:false}    }}
test(S00){x  :int => f():type{x=1} => {    if(x=1){}else{:false},f[]}}
test(S00){x  :int => f():type{x=1} => {    if(x=1){}else{:false},f[]}}
test(F00){x  :int => f():type{x=1} => {    if(x=2){:false}else{}    }}
test(S00){x  :int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(S00){x  :int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}                }}
test(S00){x&y:int => f():type{x=y} => {f[],if(x=y){}                }}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}            ,f[]}}
test(F00){x&y:int => f():type{x=y} => {    if(x=y){}else{:false}    }}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}else{:false},f[]}}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}else{:false},f[]}}

# Conditions and assumptions.
test(D00){x:int => f():type{x=1} => {x=1    }}
test(S00){x:int => f():type{x=1} => {x=1,f[]}}

# Dependent conditions.
test(S00){x:int => {i:=if(x=1){2}else{3},if{i=2}}}

# Conditional promote-assuming.
test(S00){a:int => if{a=1}}
test(S00){a:int => if(x:=a=1){x}}
test(S00){a&b:int => if{a=b}}
test(D00){a&b:int => a=b where if{a=b}}

# Decidable conditions.
test(U00){f(x:any):=if(x=2){3}else{4}}	#TODO:10.4:CHECK-COMPARE-ANY
test(S00){f(x:int):=if(x=2){3}else{4}}
test(N00){f(x:int):=if(x=1){y}else{2}}
test(N00){f(x:int):=if(x=1){1}else{y}}
test(F00){f(x:int):=if(x=1){1}else{2}=1}
test(F00){f(x:int):=if(x=1){1}else{2}=2}
test(F00){f(x:int):=if(x=10){20}else{30}=30}
test(S00){f(x:int):=nat[if(x=1){1}else{2}]}
test(S00){a:=1|2 => if(a=1){a=1}}
test(S00){a:=1|2 => if(a=1){a=1}else{not a=1}} # S00 because exact.
test(S00){a:=1|2 => if(a=1){a=1}else{a=2}}  # Same.
test(S00){a:=1|2 => if{a=1}else{a=2}}       # Same.

# Domain decidability.
test(D00){a&b:int => f(a):=9 => {a=b,f[b]}}

# The assume{..} macro.
test(D00){a:int => {assume{a},a=7}}
test(U00){a:any => {assume{a},a=7}}
test(D00){a&b&c:int => {x:=a=b=c,assume{x=b}}}
test(D00){a&b&c:int => {x:=a=b,assume{x=b},x=c}}
test(S00){a:int => {x:=a,assume{a=7},x=7}}
test(S00){a:int => {x:=a,assume{x=7},x=7}}
test(F00){a:int => {x:=a,assume{x=7},x=8}}
test(S00){a:int => {x:=7,assume{x=a},x=a}}
test(S00){a:int => {x:=a,x=7,assume{a=7}}}
test(S00){a:int => {x:=a,x=7,assume{x=7}}}
test(F00){a:int => {x:=a,x=8,assume{x=7}}}
test(S00){a:int => {x:=7,x=a,assume{x=a}}}
test(S00){a&b:int => {x:=a=b,assume{x=a}}}
test(S00){a&b:int => {x:=a=b,assume{x=b}}}
test(S00){a&b:int => {x:=a=b,assume{x=a}}}
test(S00){a&b:int => {x:=a=b,assume{x=b}}}
test(S00){a&b:int => {assume{x=a},x:=a=b}}
test(S00){a&b:int => {assume{x=b},x:=a=b}}
test(S00){a&b:int => {assume{x=a},x:=a=b}}
test(S00){a&b:int => {assume{x=b},x:=a=b}}
test(S00){r:=assume{x},x:=r,x=3}
test(S00){a&b:int => {x:=a=b,assume{x=a},assume{x=b}}}
test(S00){x:=x=assume{x}}
test(D00){a&b:int => f():=a => f()=b}
test(S00){a:int => {f():=assume{a=3}}}
test(D00){a:int => {f():=assume{a=3}; a=3}}
test(S00){a:int => {f():=assume{a=3}; f[]; a=3}}
#Broken by MakeAssumeOp input-output uniformity:
#test(S00){a:int => x:=x=assume{x}=a}
#test(D00){a&b:int => x:=x=assume{x}=a=b}

# The verify{..} macro. May not withstand input-everywhere.
test(QS00){verify{123}}
test(QF00){verify{1=2}}
test(QS00){assume{123} => 456}
test(QS00){verify{123}=123}
test(QF00){verify{123}=456}
test(QD00){a:int => verify{a=3}}
test(QD00){a:int => verify{123}=a}
#test(S00){verify{123} => 456}
#test(F00){verify{1=2} => 456}
#test(S00){verify<decides >{123} => 456}
#test(V00){verify<decides >{1=2} => 456}
#test(V00){verify<fails   >{1=2} => 456}
#test(S01){verify<fails   >{122} => 456}

# Commas and semicolons.
test(S00){a:=(1),a=1}
test(S00){a:=(1;),a=1}
test(S00){a:=(1,2),a=(1,2)}
test(S00){a:=(1,2;),a=(1,2)}
test(S00){a:=(1;2;),a=(1,2)}
test(S00){for(0){1}=array{1}}
test(S00){for(0|1){3,4}=((3,4),(3,4))}
test(S00){let(){2,3}=(2,3)}
test(S00):
	x:=let():
	  2,3
	x=(2,3)
test(S00){let(x:=1,y:=2){y=2;x=1;}}
test(S00){let(){2;3}=3}
test(S00){for(0|1){3;4}=(4,4)}
test(S00){f:=function(){r:=let(x:=3){y:=x; x+1}},f()=4}
test(S00){for{3;4}=array{4}}
test(S00):
	x:=let():
	  2
	  3
	x=3
test(S00){x:={1;2};x=2}
#LA test(F00):
#LA 	x:=
#LA 		1
#LA 		2
#LA 	x=1
#LA test(S00):
#LA 	x:=
#LA 		1
#LA 		2
#LA 	x=2
test(S00){f:=function(){2},f()=2}
test(F00){f:=function(){2},f()=3}
test(S00){f:=function(){2;},f()=2}
test(S00){f:=function(){2;3;},f()=3}
test(F00){f:=function(){2;3;},f()=2}

# Magic array solving.
#test(S00){Length(for{array{2,4,6}[i],i:=1})=1} # Parameter degeneracy issue that was hidden by virtual call matching.
test(S00){Length(for{array{2,4,6}[i:int]=4})=1}
test(S00){Length(for{array{2,4,6}[i:int]=5})=0}
test(S00){Length(for{array{2,4,6}[a:any]=5})=0}
test(S00){if{i where array{2,4,6}[i:int]=4}=1}

# Decidable conditions that later succeed.
test(S00){a:int => f():type{a=3} => {if(a=3) then 123 else :false, f()}}
test(F00){a:int => f():type{a=3} => {if(a=4) then 123 else :false, f()}}
test(F00){a:int => f():type{a=3} => {if(a=3) then :false else 123, f()}}
test(S00){a:int => f():type{a=3} => {if(a=4) then :false else 123, f()}}

# Dependence on parent domain resolves effect.
test(P00){f(x:int):=x => f(:int)=3}
test(S00){a&b:any => f(:any)<computes>:any => c:=f[a] => d:=f[b] => a=b => c=d}
test(A00){f(:int):int => {x:=f[x],if(int[x]) then assume{x=3}}}

# Allow.
test(R00){allow{:int}}
test(QA00){allow<resolves>{:int}}

# Conditions lacking runtime semantics.
test(A00){x:int,if(x=3) then x=3 else x=4}
test(A00){x:int,if(int[x]) then x=3 else x=4}
test(S00){f():type{x:int,if(x=3) then x=3 else x=4} => f()}
test(A00){x:int,if(x=3) then x=3 else x=4}
test(A00){x:int,if(nat[x]){x=1}}
test(A00){x:int,y:int,if(x=y){x=y=1}}
test(A00){x:any,if(int[x]){x=1}}
test(A00){x:int,if(int[x]){x=1}}
test(A00){x:int,if(int[x]) then 1 else 2}
test(A00){x:int,if(nat[x]) then 1 else 2}
test(A00){x:int,if(int[x]) then x=1 else x=2}
test(A00){x:int,if(nat[x]) then x=1 else x=2}
test(A00){x:int,if(x=1){x=1}else{x=1}}
test(A00){x:int,if(x=1){x=1}else{x=2}}
test(A00){if{nat[x:=x]}}
test(A00){if{x:=nat[y],y:=nat[x]}}

# Unsound condition behaviour due to constant degeneracy.
#test(S00){array{0,1} where if{1=0 where array{0,1}}}

# Interesting conditions.
test(S00){a:int => f(x:int):int => f(a)}
test(S00){a:int => f(3):int => f[:int]}
test(S00){a:int => g(:nat):int => f(x:int):int => if{f[g[a]]}}

# Unify macro.
test(S00){a:=operator'='{}}
test(S00){a:=operator'='{2},a=2}
test(F00){a:=operator'='{2},a=3}

# Various.
test(F00){a:int => {x:=a,assume{x=7},x=8}}
test(F00){a:int => {x:=a,x=8,assume{x=7}}}
test(F00){f(x:int):=array{x,2}=array{3,x}}
test(U00){f(x:int,y:any):=array{x,2}=array{3,y}}
test(F00){x:int => f():=x=1 => {x=2,f[]}}
test(F00){x:int => if(x=1|2){x=1}}
test(F00){x:int => if(x=1|2){x=2}}
test(F00){x:int => if(x=1|2){x=3}}
test(F00){x:int => if(x=1|3|2){x=1 or x=2}}
test(S00){x:int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(S00){x:int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(F00){a:int => f():type{a=1} => {a=2,f[]}}

# Array heads.
test(F00){x:=2 where array{x}=array{3}}
test(F00){x:int where array{x,2}=array{3,x}}
test(F00){x:=x where array{x,2}=array{3,x}}
test(F00){f(x:int):=array{x,2}=array{3,x}}

# Trivial tests.
test(S00){1=1}
test(F00){1=2}
test(S00){1+2=3}

# Arrays.
test(F00){(10,20)=(10,20,30)}
test(S00){(10,20)=(10,20)}
test(D00){a:int => (10,20)=(10,a)}
test(U00){a:any => (10,20)=(10,a)}
test(F00){(10,20)=(11,20)}
test(F00){(10,20)=(10,21)}
test(S00){as:=(:int,:int) => bs:=(11,21) => as(0)=bs(0) => as(0)=bs(0)}
test(D00){as:=(:int,:int) => bs:=(11,21) => as(0)=bs(0) => as(1)=bs(1)}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as=bs}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as(0)=bs(0)}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as(1)=bs(1)}
test(S00){a:int => as:=(10,20) => bs:=(a,20) => as=bs => as=bs}
test(F00){as:=(10,20) => bs:=(11,20) => as=bs}
test(S00){x=array{20},x:=array{20}}
test(S00){for{a}=array{20} where a:=20}

# Unification comparability.
test(D00){f(x:int,y:int):=array{x,2}=array{3,y}}
test(F00){f(x:int):=array{x,2}=array{3,x}}
test(U00){f(x:int,y:any):=array{x,2}=array{3,y}}

# Trivial for.
test(S00){for{}=array{array{}}}
test(S00){for{1}=array{1}}
test(F00){for{1}=array{}}
test(S00){for{2=2}=array{2}}

# Trivial for.
test(F00){for{2=2}=array{3}}
test(S00){for{1=2}=array{}}

# Forking for.
test(S00){for{1|2}=array{1,2}}
test(S00){for{1|2|3}=array{1,2,3}}
test(S00){for{1|(2=3)|3}=array{1,3}}

# Binding for.
test(S00){for(x:=1){x}}
test(S00){for(x:=1|2){x}}
test(S00){for(x:=1|2){x}=array{1,2}}
test(S00){for(x:=1|2|3){x}=array{1,2,3}}
test(S00){for(a,1|2){-a},a:=3}

# Combinatorially costly.
#NEWREGS test(S00){for(x:=1|2|3|4,not x=2|3){x}=array{1,4}}
#NEWREGS test(S00){for(x:=1|2|3,not x=2){x}=array{1,3}}
test(S00){for(x:=2|3|4,not x=2|3){x}}

# Multi for.
test(S00){for(x:=10|20,y:=1|2){x+y}=array{11,12,21,22}}
test(S00){for(x:=a|b,y:=1|2){x+y}=array{11,12,21,22},a:=10,b:=20}
test(S00){operator'+'(for(x:=1|2){x})=3}
test(S00){for{operator'+'(0|2,0|1)}=array{0,1,2,3}}
test(S00){Length(for{let(){0|1|2}=1})=1}
test(S00){for{0|2,0|1}=((0,0),(0,1),(2,0),(2,1))}
test(S00){for{operator'+'(for(x:=2|1){0|x})}}

# Unification strength.
test(S00){a:int => (as:=(a,1),bs:=(a,1)) => as=bs}
test(D00){a&b:int => (as:=(a,1),bs:=(b,1)) => as=bs}
test(S00){a&b:int => (as:=(a,1),bs:=(b,1)) => as=bs => as=bs}

# Failure in iteration context after initial success and promotion.
test(F00){for{400|200}=(401,200)}
test(F00){function(100 where for{400|200}){300}[100]=400}
test(F00){function(100 where for(400|200){500}){300}[100]=400}
test(F00){function(for(x:=3|4){x}){8}[5,4]}

# Domain inclusion.
test(S00){a:any   => f(:int):any => f[a] => f[a]}
test(S00){a:any   => f(:int):any => f[a] => a=3 => f[3]}
test(S00){a&b:any => f(:int):any => f[a] => a=b => f[b]}

# Function abstraction tests.
test(S00){function(1){2}[1]}
test(F00){function(1){2}[0]}
test(S00){function(x:prefix'-'){x}(3)=-3}
test(S00){function(x:=-3){x}[-3]}
test(F00){function(-3){4}[3]}
test(S00){function(let(1){2}){4}[2]}
test(F00){function(let(1){2}){4}[3]}
test(S00){a:=2,function(a){3}[2]=3}
test(S00){function(x:int,y:prefix'-'){x,y}[3,4]=(3,-4)}
test(S00){function(for(x:=3|4){5}){6}[5,5]=6}
test(S00){function(for(x:=3|4){x+1}){7}[4,5]=7}
test(S00){function(for{3|4}){8}[3,4]}
test(S00){function(as:=for(x:=1|2){3}){as}[3,3]=(3,3)}
test(S00){function(as:=for(x:=1|2){:int}){as}[3,4]=(3,4)}
test(S00){function(as:=for(x:=1|2){:prefix'-'}){as}[3,4]=(-3,-4)}
test(S00){function(1 where as:=for{x:=1|2|3}){as}[1]=(1,2,3)}
test(F00){function(for{3|4}){8}[4,5]}

# Container iteration.
test(S00){for(x:array{}){x+1}=array{}}
test(S00){for(x:array{1}){x+1}=array{2}}
test(S00){for(x:array{3,5}){x+1}=array{4,6}}
test(S00){for(x:array{1,3,2,4,3,5,4,6}){x}=array{1,3,2,4,3,5,4,6}}
test(S00){for{::((1,2),(3,4))}=(1,2,3,4)}
test(S00){for(xs:((1,2),(3,4)),x:xs){x}=(1,2,3,4)}

# For and prefix':'.
test(S00){for(x:=10|20,y:=1|2){x+y}=array{11,12,21,22}}
#unsound test(S00){for(x:xs,y:=1|2){x+y}=array{11,12,21,22},xs:=(10,20)}
test(S00){for(x:=10|20,y:ys){x+y}=array{11,12,21,22},ys:=(1,2)}
#unsound test(S00){for(x:xs,y:ys){x+y}=array{11,12,21,22},ys:=(1,2),xs:=(10,20)}
test(S00){for(x:xs,y:ys){x+y}=array{11,12,21,22},xs:=(10,20),ys:=(1,2)}

# Ranges.
test(S00){for{1..3}=array{1,2,3}}
test(S00){for{3..7}=array{3,4,5,6,7}}
test(S00){for{1|3..7|12}=array{1,3,4,5,6,7,12}}
test(S00){for(x:=1..5) do Print(x)}

# Choice.
test(S00){for{(1|2)=3}=array{}}
test(S00){for{(1|2)=1}=array{1}}
test(S00){for{(1|2)=2}=array{2}}
#NEWREGS test(S00){for{(1|2)=(1|2)}=array{1,2}}
#NEWREGS test(S00){for{(1|2)=(2|1)}=array{1,2}}

# Choice requiring new scopes.
test(S00){for{x:=1|(2,3),y:=(3,4)|2}}
test(S00){for{x:=1|(a:=1,a),y:=(b:=1,b)|2}}
test(N00){for{x:=1|(a:=1,a),a}}

# Condition freedom.
test(S00){a&b:int => if(a=b){a=b}}
test(S00){a:int => {x:=a,x=7 => x=7}}
test(S00){a:int => {x:=a,x=a => x=a}}
test(S00){a:int => {x:=7,if(x=a){x=a}}}
test(S00){a:int => {x:=a,if(x=7){x=7}}}
test(S00){a&b:int => {x:=a,if(x=b){x=b}}}
test(S00){a&b&c:int => {x:=a,if(x=b){x=a,x=b}}}

# Flexible assumptions.
test(S00){a&b:any => {x:=a=b,assume{x=a}}}
test(QD00){a&b:any => {x:=a=b,assume<decides>{x=a}}}
test(S00){a&b:any => {x:=a=b,assume{a=b}}}
test(QD00){a&b:any => {x:=a=b,assume<decides>{a=b}}}

# Misc.
test(S00){a:int => f(x:int):int => if{f[a]=1}}
test(S00){a:int => f(x:nat):int => if{f[a]}}
test(S00){a:int => f(x:nat):int => if{f[0]=1}}
test(S00){a:int => f(7    ):int => if{f[a]}}
test(S00){a:int => f(7    ):int => if{f[7]=1}}
test(D00){f(:int):int => f[0]=f[1]}
test(S00){a:int => f(7):int => if{f[a]=1}}
test(S00){a:int => f(x:nat):int => if{f[a]=1}}

# Assumed and unassumed native types.
test(S00){x:int,x=3}
test(F00){x:int,x=()}
test(S00){x:int => int[x]}
test(D00){x:int => nat[x]}
test(S00){a:int => {x:int,x=a}}
test(D00){a:int => {x:nat,x=a}}
test(F00){x:int,x='x'}

# Back to the basics.
test(D00){x:=assume{:int},x=3}
test(U00){a:=a => {x:=a=3,assume{a}}}

# Fixed point convergence.
test(S00){a&b:int => f(a):type{a=b} => {f[a],a=b}}
test(S00){a&b:int => f(x&y:any):=0 => {x:=a,y:=b,f[x,y]}}
test(S00){a&b:int => f(x&y:any):=x => {x:=a,y:=b,f[x,y]}}
test(S00){a&b:int => f(x&y:any):=y => {x:=a,y:=b,f[x,y]}}
test(S00){a:int => f(x:any):type{a=3} => f[a]}
test(D00){a:int => f(x:int):int => f[a]=3}
test(D00){a&b:int => f(a):type{a=b} => {f[x],x:=b}}	#TODO:10.4:CHECK-STRENGTH:TYPE (duplicate with l591?)
test(D00){a&b:int => f(a):type{a=b} => {x:=b,f[x]}}
test(S00){a:int => f(x:nat):int => if{f[a]=1}}
test(S00){a:int => f(x:int):type{x} => f[a]=a}
test(D00){a:int => f(x:int):=x => f[a]=3}
test(S00){a:int => f(x:any):type{x=3} => f[a]}
test(S00){a&b:int => f(x&y:any):type{x=y} => {x:=a,y:=b,f[x,y]}}
test(D00){a&b:int => f(x:int):=x => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y]}}
test(A00){:int}

# Unification strength.
test(S00){f(x:int):=x => a:int => f[a]=f[a]}
test(D00){f(x:int):=x => a&b:int => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(D00){f(x:int):=x => a&b:int => {f[a]=f[b],a=b}}

# Testing existing features.
test(S00){x:int => int[x]}
test(D00){x:int => nat[x]}
test(S00){x:any => any[x]}
test(U00){x:any => int[x]}

# Assumptions about flexible unificands.
test(S00){a&b:any => {x:=b,     assume{x=a}}}
test(S00){a&b:any => {x:=a=b,   assume{x=a}}}
test(S00){a&b:any => {x:=a=b,   assume{x=b}}}
test(S00){a&b:any => {x:=b,x=a, assume{x=a}}}
test(S00){a&b:any => {x:=b,x=a, assume{x=b}}}
test(S00){a&b:any => {x:=a=b,   assume{x=a},assume{x=b}}}
test(S00){a&b:any => {x:=b,x=a, assume{x=a},assume{x=b}}}

# Cyclic contingencies.
test(S00){a:int => f(x:int):=x => {x:=a,f[x]}}
test(S00){a:int => f(x:int):=x => {f[x],x:=a}}

# Nesting.
test(S00){a:int => f(x:nat):int => if{x:=f[a]}}
test(S00){a:nat => f(x:nat):int => if{x:=f[a],int[x]}}

# Flex Assumptions.
test(D00){a&b:int => f(x&y:int):=x=y => {f[a,a],a=b}}
test(D00){a:int   => f(x&y:int):=x=y => {f[a,a],a=2}}
test(U00){a:any   => f(x&y:any):=x=y => {f[a,a],a=2}}

# Code lacking runtime semantics.
test(A00){:int}

# Contingent decidability.
test(S00){a&b&c:int => f(x:int):type{a=b} => f[3]}
test(D00){a&b&c:int => f(c    ):type{a=b} => f[3]}
test(S00){a&b:int => f(x&y:any):type{x=y} => {x:=a,y:=b,f[x,y]}}
test(S00){a&b&c:int => f(:int):type{a=b} => {x:=a,y:=b,f[3]}}
test(D00){f(:int):int => f[3]=f[4]}

# Decidability Propagation.
test(U00){a&b:int => f(:int)<computes>:any => x:=f(a) => y:=f(b) => if{f(a)=f(b)}}
test(S00){a&b:int => f(:int)<computes>:any => x:=f(a) => y:=f(b) => if{a=b,f(a)=f(b)}}

# Simple comparability.
test(S00){a:int   => f(x:nat):int => if{f[a]=1}}
test(D00){a&b:int => f(x:int):=x => {f[a]=f[b]}}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}

# Verify whole function domain.
test(F00){function(x:=3|5){x+1=4}}
test(F00){function(x:=3|5){x+1=6}}

# Simple clustering tests.
test(S00){1}
test(S00){1=1}
test(F00){1=2}
test(S00){f():=1}
test(S00){f(1):=2,f[1]}
test(S00){array{}}
test(S00){2=2}
test(S00){f():=2,f[]}
test(S00){int}
test(S00){int[2]}
test(D00){i:int => i=2}
test(S00){f(x:int):=x,f[3]+1=4}
test(S00){f(x:int):=x,f[3]=3}
test(F00){f(x:int):=x,f[3]=4}
test(S00){f(x:int,y:int):=0 => f(3,5)}
test(S00){()=()}
test(S00){(1,2)=(1,2)}

# Patches.
test(D00){f(x:int):int => a:int => f[a]=a}
test(D00){f(x:int):int => s:=7 => f[s]=s}
test(D00){f(x:int):int => f[7]=7}

# Unification strength.
test(S00){f(x:int):=x => a:int => f[a]=f[a]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(D00){f(x:int):=x => a&b:int => {f[a]=f[b],a=b}}

# Synthetic.
test(S00){a&b:any => {assume{a=b},a=b}}
test(S00){a&b:any => {a=b,assume{a=b}}}
test(QD00){a&b:any => {assume<decides>{a=b}}}
test(QD00){a&b:any => {assume<decides>{a=b},a=b}}
test(QD00){a&b:any => {a=b,assume<decides>{a=b}}}
test(U00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d}}}
test(QD00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{b=c}}}
test(QD00){a&b&c&d:any => {assume<decides>{a=b},assume<decides>{c=d},assume{a=c}}}
test(S00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},b=d,assume{a=c}}}
test(S00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume{a=c},b=d}}

# Would need inclusion-level context granularity to make these succeed.
test(QS00){assume<decides>{1}}
test(QS00){a&b:any     => {assume{a=b},assume<decides>{a=b}}}
test(QD00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{a=c}}}
test(QD00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{b=d}}}
test(QS00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{b=d},assume{a=c}}}
test(QS00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume{a=c},b=d}}

# Not matching.
test(U00){x:=x => y:=y => x=y}
test(U00){x:any => y:any => x=y}
test(U00){x:int => y:any => x=y}
test(D00){x:int => y:int => x=y}

# Assumption strength.
test(F00){f(x:=0):=x => f[1]}
test(F00){x:int where array{x,2}=array{3,x}}
test(F00){x:int => f():=x=1 => {x=2,f[]}}
test(D00){a&b:int => c:any => f():type{c=a} => {f[],c=b}}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y) => f[0,3]}

# Funky unification.
test(S00){int[123]}
test(S00){x:=123=x}
test(S00){x:=x=123}
test(S00){int[a:any]=123}
test(S00){x:=int[x]=123}

# Named abstraction inputs.
test(S00){a->b:=2,a=2,b=2}
test(S00){a->b:=c:=3,a=3,b=3,c=3}
test(S00){function(a->b:prefix'-'){a,b}[3]=array{3,-3}}
test(S00){for(i->x:(0,10,20)){i+x}=array{0,11,22}}

# Where.
test(S00){1 where 2}
test(S00){x:=1 where y:=x+1}
test(S00){x:=y+1 where y:=1}
test(S00){(1 where 2)=1}
test(F00){(1 where 2)=2}
test(S00){(x:int where x+1=3)=2}
test(F00){(x:int where x+1=3)=1}

# Function logic.
test(S00){function(){}}
test(F00){function(){1=2}}
test(S00){function(x:=1){x=1}}
test(F00){function(x:=1){x=2}}

# Conclusion forking.
test(S00){function(x:=1|2){x}}
test(F00){function(x:=1|2){x=1}}
test(F00){function(x:=1|2){x=2}}
test(F00){function(x:=1|2){x=3}}
test(S00){function(x:=1|2){int[x]}}
test(S00){function(x:=1|2){Length(for{x=1|2})=1}}

# Simple condition tests.
test(F00){function(a:=if(1=2){3}else{4}){a}[3]}
test(S00){function(a:=if(1=2){3}else{4}){a}[4]}
test(S00){function(a:=if(1=1){3}else{4}){a}[3]}
test(F00){function(a:=if(1=1){3}else{4}){a}[4]}

# Lambda.
test(S00){(x:int => x)[2]=2}

# operator'&'
test(S00){a&b:=3}
test(S00){a&b:=3,a=3,b=3}
test(F00){a&b:=3,a=4}
test(F00){a&b:=3,b=4}

# Unification and flexibility.
test(S00){a&b:int => a=a}
test(D00){a&b:int => a=b}
test(S00){a:int => {x:=x,x=a}}
test(S00){a:int => {x:=x,a=x}}
test(S00){a:int => {x:=x,x=a,a=x}}
test(D00){a&b:int => {x:=x,x=a,x=b}}

# Assumption inflexibility.
test(D00){f:=function(a:int){a=3}}
test(D00){f:=function(a:int,b:int){a=b}}

# Assumed functions.
test(S00){1 => 2 => 3}
test(S00){(1 => 2) => 3}
test(S00){f(x:int):=1 => 3}

# Call unification, flexibility, matching.
test(S00){(f(x:int):=1) => f(0)}
test(U00){f(x:int):type{y:=y} => f[0]=f[1]}

# Clause resolution.
test(S00){function(1){1+2;3+4}}
test(S00){let(){1+2;3+4}}

# Degrees of freedom.
test(F00){a:int => f():=a=1 => {a=2,f[]}}

# Calls.
test(C00){1(2)}
test(C00){1[2]}
test(P00){false(2)}
test(F00){false[2]}
test(C00){1[2] => 3}
test(C00){f:any => f[2] => 3}
test(C00){1(2) => 3}
test(C00){f:any => f(2) => 3}
test(S00){f(:int):int => f(2) => 3}
test(P00){f(:int):int => f() => 3}
test(S00){f:=(1,2,3) => f[2] => 3}
test(S00){f(:int):int => f[2] => 3}
test(S00){(f(2):=1,f[2]) => 3}

# Why does error not display suspensions?
test(F00){1=2}
test(F00){:false}
test(F00){f():=1=2}
test(F00){f():false}

# Abstract head shape rejection.
test(QF00){a:int => b:float => a=b}
test(F00){a:int => a=(3,4)}
test(QF00){a:float32 => b:float64 => a=b}
test(F00){a:int => a=int}
test(F00){a:int => a='3'}
test(F00){a:int => '3'=a}
test(D00){a:int => a=3}
test(D00){a:int => 3=a}

# Unification comparability.
test(S00){f(x:any):=x=x}
test(S00){f(x:int):=x=x}
test(D00){f(x&y:int):=x=y}
test(U00){f(x:int,y:any):=x=y}
test(U00){f(x&y:any):=x=y}
test(D00){f(x:int):=x=2}
test(F00){f(x:int,y(2):=3):=x=y}
test(U00){f(x:int,y:any):={x=2,y=3}}
test(U00){f(x:int,y:any):={y=3,x=2}}
test(S00){int[a:any] => int[a]}
test(D00){int[a:any] => nat[a]}

# Dependency.
test(S00){f(x:int,x):=x,f[3,3]}
test(F00){f(x:int,x):=x,f[3,4]}

# Numbers.
test(S00){nat[123]}
test(F00){nat[-123]}
test(S00){nat[4/2]}
test(F00){nat[2/3]}		#TODO:10.4:RATIONALS
test(S00){int[123]}
test(S00){int[-123]}
test(F00){int[2/3]}		#TODO:10.4:RATIONALS
test(QS00){rational[123]}
test(QS00){rational[-123]}
test(QS00){rational[2/3]}
test(QS00){rational[-2/3]}

# Basic domain matching and comparability.
test(S00){f(x:int):=int[x]}
test(D00){f(x:int):=char[x]}
test(S00){t(x:int     ):=x,f(x:t):=t[x]}
test(U00){t(x:int     ):=x,f(x:any):=t[x]}
test(S00){f(x:any     ):=any[x]}
test(S00){f(x:int     ):=any[x]}
test(QU00){f(x:any     ):=rational[x]}
test(QS00){f(x:nat     ):=rational[x]}
test(QS00){f(x:int     ):=rational[x]}
test(QS00){f(x:rational):=rational[x]}
test(U00){f(x:any     ):=int[x]}
test(S00){f(x:nat     ):=int[x]}
test(S00){f(x:int     ):=int[x]}
test(QD00){f(x:rational):=int[x]}
test(U00){f(x:any     ):=nat[x]}
test(S00){f(x:nat     ):=nat[x]}
test(D00){f(x:int     ):=nat[x]}
test(QD00){f(x:rational):=nat[x]}
test(S00){f(x:void    ):=x=false,f[123]}
test(F00){char[1]}
test(S00){char['X']}
test(QS00){char8['X']}

# Function checking. Now fuzzy.
test(S00){f(g(1):=1):=2,f[h(1):=1]}
test(U00){f(g(1):=1):=2,f[h(2):=1]}
test(U00){f(g(1):=1):=2,f[h(1):=2]}
test(S00){f(g(1):=1):=2,f[h(:int):=1]}
test(S00){f(g(1):int):=2,f[h(:int):=1]}
test(U00){f(g(1):=1):=2,f[h(:int):=2]}
test(U00){function(g(1):=2){g}[function(0){2}]}
test(F00){function(g(1):=2){g}[function(0){2}][1]}

# Function abstraction.
test(S00){function(g(1):=2){g}[function(1){2}]}
test(S00){function(g(1):=2){g}[function(1){2}][1]=2}

# Full function plumbing.
test(S00){function(g(1):=2){g}[function(x:int){2}][1]=2}
test(F00){function(g(1):=2){g}[function(x:int){2}][1]=3}
test(F00){function(g(1):=2){g}[function(0    ){2}][1]}
test(F00){function(g(1):=3){g}[function(x:int){2}][1]}
test(S00){function(g(1):int){g}[function(:int){2}][1]=2}
test(F00){function(g(1):int){g}[function(:int){2}][1]=3}
test(F00){function(g(x:int):int){g}[function(:any){2}][]}	#TODO:10.4:WHY??? rejected by 10.5 / implies-direct with direct

# Misc.
test(S00){1}
test(S00){1,2}
test(S00){1+2}
test(S00){1=1}
test(F00){1=2}
test(S00){1+2=3}
test(F00){1+2=4}
test(S00){x+2,x:=1}
test(S00){x:=1,if(2){3}else{4}=3}
test(F00){x:=1,if(1=2){3}else{4}=3}
test(S00){x:=1,if(1=2){3}else{4}=4}
test(S00){x:=1,if(x=1){3}else{4}}
test(S00){x:=1,if(x=2){3}else{4}}
test(S00){x:=1,if(x=2){3}else{4}=4}
test(F00){x:=1,if(x=2){3}else{4}=5}
test(S00){array{2,3,4}[1]=3}
test(S00){if{2=2}}
test(S00){function(x:=x){x}}
test(S00){function(x:=x){x=x}}
test(S00){function(x:=x,y:=x){x=y}}
test(U00){function(x:=x){x=1}}
test(U00){function(x:=x,y:=y){x=y}}
test(S00){if(1=1) {x:=2} else {y:=3}}
test(S00){if(1=2) {x:=2} else {y:=3}}
test(S00){x:=if(1) then 2 else 3,x=2}
test(S00){x:=3,-x}
test(S00){-x,x:=3}
test(A00){x:='a',-x} # Could be U00 but missing axiom.
test(A00){-'a',x:=3} # Same.

# In order symbols.
test(S00){x:=1,x=1}
test(F00){x:=1,x=2}
test(F00){x:=1,int[x=2]}
test(F00){x:=1,y:=x=2}

# Out of order.
test(S00){x=1,x:=1}
test(F00){x=1,x:=2}

# Let.
test(S00){let(x:=2,y:=3){y+x}=5}

# Identifier not found.
test(N00){x}
test(N00){let(x:=1){},x}
test(N00){let(){x:=1},x}
test(N00){let(x){x:=1}}
test(N00){function(x){1}}
#LA test(QN01){x{}}
#LA test(QN01){allow{x{}}}
test(QN00){assume<decides>{bogus}}
test(N00){let(x:=2){y:=x+1},x=2}
test(N00){let(x:=2){y:=x+1},y=3}
test(S00){y:=let(x:=2){x+1},y=3}

# Duplicate identifier.
test(N04){x:=0,x:=1}
test(N04){x:=0,x:int=>3}
test(N04){x:=0,y:int=>x:=1}

# Not.
test(S00){not 1=2}
test(S00){not (1=2)}
test(F00){not 1=1}
test(F00){not 1}
test(F00){not 1|2}	#TODO:10.4:NOT

# Nested choice.
test(S00){if((1|2)=1){4}else{5}=4}
test(S00){if((1|2)=2){4}else{5}=4}
test(S00){if((1|2)=3){4}else{5}=5}

# Nested choice with symbol resolution.
test(S00){if(x:=(1|2),x=1){4}else{5}=4}
test(S00){if(x:=(1|2),x=2){4}else{5}=4}
test(S00){if(x:=(1|2),x=3){4}else{5}=5}

# Nested choice out-of-order.
test(S00){Length(for{x=1,x:=1|2})=1}
test(S00){Length(for{x=2,x:=1|2})=1}
test(S00){Length(for{x=3,x:=1|2})=0}
test(S00){if(x=1,x:=(1|2)){4}else{5}=4}
test(S00){if(x=2,x:=(1|2)){4}else{5}=4}
test(S00){if(x=3,x:=(1|2)){4}else{5}=5}

# For.
test(S00){for(x:=888|999) do x}

# Functions.
test(S00){function(1 where a:=2){a}[1]=2}
test(F00){function(1 where a:=2){a}[1]=3}

# Requires call matching.
test(QD00){a&b:int => f(:int)<computes>:any => allow{a=b} where f[a]=f[b]}
test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=as}
test(S00){f(x:any)<computes>:any => f[3]=f[3]}
test(S00){f(:any)<computes>:any => f(2)=3 => f[2]=3}

# Tests that were dependent on applying constraints non-locally to function.
test(F00){f(:any)<computes>:any => f[2]=(_,_) => f[2]=(_,_,_)}
test(D00){(f(x:int)<computes>:int,f[3]=4)=>f[4]=5}
test(S00){(f(x:int)<computes>:int,f[3]=4)=>f[3]=4}
test(F00){(f(x:int)<computes>:int,f[3]=4)=>f[3]=5}
test(QD00){a&b:int => f(:int)<computes>:any => allow{a=b} where f[a]=f[b]}
test(F00){f(:any)<computes>:any=>f[2]=3=>f[2]=4}
test(F00){f(7)<computes>:type{y:=y} => f[7]=8 => f[7]=9}
test(F00){a:any => f(:any)<computes>:any => f[a]=1 => f[a]=2}
test(F00){f(:any)<computes>:any => f[2]=3 => f[2]=4}

# Asynchronous matching.
test(S00){f(:int)<computes>:int => a:int => {y:=a,f(a)=f(y)}}
test(S00){f(:int)<computes>:int => a:int => {f(a)=f(y),y:=a}}
test(S00){f(:int)<computes>:int => a:int => {f(3)=f(y),y:=3}}
test(D00){f(:int)<computes>:int => a&b:int => {f[a]=f[y],y:=b}}
test(U00){f(:int)<computes>:any => a&b:int => {f[a]=f[y],y:=b}}
test(S00){f(:int)<computes>:int => a&b&c:int => f(a)=b => f(a)=c => b=c}

# Call matching.
test(D00){f(x:int)<computes>:int => f(0)=f(1)}
test(S00){f(x:int)<computes>:int => f(0)=f(0)}
test(S00){f(x:int)<computes>:int => {x:=f(0),g():=x=f(0)}}
test(S00){f(x:int)<computes>:int => {x:=f(0),y:=f(0),g():=x=y}}
test(S00){f(x:=x)<computes>:type{y:=y} => a:=f[3] => b:=f[3] => a=b}
test(S00){f(x:=x)<computes>:type{y:=y} => a:=f[3] => b:=f[3] => b=2 => a=2}
test(S00){x&y:int => f(:int)<computes>:int => a:=f[x] => b:=f[y] => x=y => a=b}
test(S00){as:=array{x:=x} => bs:=array{y:=y} => as=bs => as[0]=bs[0]}
test(U00){f(x:=x)<computes>:type{y:=y} => a:=f[3] => b:=f[4] => a=b}
test(U00){f(x:=x)<computes>:type{y:=y} => a:=f[3] => b:=f[4] => b=2 => a=2}
test(S00){f(x:=x)<computes>:type{y:=y} => f[0]=f[0]}
test(U00){f(x:=x)<computes>:type{y:=y} => f[0]=f[1]}
test(S00){f(:nat)<computes>:int => f[0]=f[0]}
test(D00){f(:nat)<computes>:int => f[0]=f[1]}
test(S00){f(:nat)<computes>:int => a:int => f[a] => f[a]}
test(D00){x&y:int => f(:int)<computes>:int => a:=f[x] => b:=f[y] => a=b}
test(D00){x&y:int => f(:int)<computes>:int => f[x]=f[y]}
test(U00){f(:int)<computes>:any => f[0]=f[1]}
test(D00){f(:int)<computes>:int => f[0]=f[1]}
test(D00){x&y:int => f(:int)<computes>:int => a:=f[x] => b:=f[y] => {x=y,a=b}}

# Past breakage.
test(D00){a:int => f(7)<computes>:type{y:=y} => f[7]=a => f[7]=9}
test(F00){f(7)<computes>:type{y:=y} => f[7]=8 => f[7]=9}
test(S00){f(x:int)<computes>:any => a&b:int => a=b => f[a]=f[a]}

# Function unification.
test(QS00){(f&g)(x:int)<computes>:int => f[3]=f[3]}
test(QD00){(f&g)(x:int)<computes>:int => f[3]=f[4]}
test(QD00){(f&g)(x:int)<computes>:int => f[3]=g[3]}
test(QS00){(f&g)(x:int)<computes>:int => f=g => f[3]=g[3]}
test(QU00){(f&g)(x:int)<computes>:int => if{f=g}}
test(QU00){(f&g)(x:int)<computes>:int => if{f[3]=g[3],f=g}}
test(U00){f(x:int)<computes>:any => a&b&c:int => if{f[a]=f[c]}}
test(S00){f(x:int)<computes>:any => a&b&c:int => if{a=c,f[a]=f[c]}}
test(S00){f(x:int)<computes>:any => a&b&c:int => a=b => if{a=c,f[a]=f[c]}}
test(S00){f(x:int)<computes>:any => a&b&c:int => a=b => if{f[a]=f[b]}}
test(S00){f(x:int)<computes>:any => a:int => if{a=3,f[a]=f[a]}}

# Atom coherency.
test(S00){f()<computes>:int => f()=f()}
test(S00){f(x:int)<computes>:int => f(1)=f(1)}
test(S00){f(x:int)<computes>:int => f(274876858367*4398042316799)=f(1208920055006831267807233)}

# Parameter effect on call equality inference. Shows context_domain logic suffices for parameters.
test(S00){a&b:int => f(:int)<computes>:any => if{a=b}}
test(QD00){a&b:int => f(:int)<computes>:any => x:=f(a) => y:=f(b) => allow{a=b} where x=y}

# Later functions and parameters and elements.
test(S00){a&b:any => f(:any)<computes>:any => c:=f[a] => d:=f[b] => a=b => c=d}
test(U00){a&b:any => f(:any)<computes>:any => c:=f[a] => d:=f[b] => {a=b,c=d}}
test(D00){a&b:int => f(:int)<computes>:any => c:=f[a] => d:=f[b] => {a=b,c=d}}
test(QS00){a:any => (f&g)(:any)<computes>:any => c:=f[a] => d:=g[a] => f=g => c=d}
test(QS00){a&b:any => (f&g)(:any)<computes>:any => c:=f[a] => d:=g[b] => a=b => f=g => c=d}

# Pure function equality.
test(S00){a&b:any   => f(:any)<computes>:any => f[a]=b => f[a]=b}
test(U00){a&b&c:any => f(:any)<computes>:any => f[a]=b => f[a]=c}
test(S00){a:any     => f(:any)<computes>:any => f[a]=1 => f[a]=1}
test(F00){a:any     => f(:any)<computes>:any => f[a]=1 => f[a]=2}
test(S00){f(:any)<computes>:any => f(2)=3 => f[2]=3}
test(F00){f(:any)<computes>:any => f(2)=3 => f[2]=4}

#
# Foolproof equality constraint tests.
#

# Simple.
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[a]               => h[x]=h[y]}
test(QD00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[b]               => h[x]=h[y]}
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[b] => a=b        => h[x]=h[y]}
test(QD00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[a]               => h[x]=h[y]}
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[a] => f=g        => h[x]=h[y]}
test(QD00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => a=b        => h[x]=h[y]}
test(QD00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => f=g        => h[x]=h[y]}
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => a=b => f=g => h[x]=h[y]}
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => f=g => a=b => h[x]=h[y]}
test(QS00){a&b:int=>(f&g&h)(:int)<computes>:int => f[a]=g[b] => h[f[a]]=h[g[b]]}

# Arrays.
test(QS00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[a,c]               => h[x]=h[y]}
test(QD00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[b,c]               => h[x]=h[y]}
test(QS00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[b,c] => a=b        => h[x]=h[y]}
test(QD00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[a,c]               => h[x]=h[y]}
test(QS00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[a,c] => f=g        => h[x]=h[y]}
test(QD00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => a=b        => h[x]=h[y]}
test(QD00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => f=g        => h[x]=h[y]}
test(QS00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => a=b => f=g => h[x]=h[y]}
test(QS00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => f=g => a=b => h[x]=h[y]}

# Function equates requiring memoization.
test(QS00){a&b:any => (f&g)(:any)<computes>:any => f[a]=g[b] => f[a]=g[b]}
test(QU00){a&b:any => (f&g)(:any)<computes>:any => f[a]=g[b] => f[a]=g[a]}
test(S00){a:any => f(:any)<computes>:any => int[f[a]] => int[f[a]]}
test(S00){a:any => f(:any)<computes>:any => nat[f[a]] => int[f[a]]}
test(D00){a:any => f(:any)<computes>:any => int[f[a]] => nat[f[a]]}

# New eta breakage.
test(D00){f(x:int)<computes>:int => f[3]=4}
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(U00){f(x:int)<computes>:type{y:=y} => f[3]=f[4]}
test(D00){f(x:int)<computes>:int => f[3]=f[4]}
test(F00){f()<computes>:=3 => g()<computes>:=4 => c:=f()=g()}
test(S00){a:any=>int[a]=>comparable[a]}

# Random stuff broken by eta.
test(S00){(1,2) where (1,2)}
test(S00){(1,2)=(1,2)}
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(D00){f(x:int):int => f[3]=f[3]} #TODO:FUN-OUT-EQ (duplicate l2328)
test(S00){f(:int,:int)<computes>:int => f[3,4] where f[3,4]}
test(S00){f(:int,:int)<computes>:int => f[3,4]=f[3,4]}
test(D00){f(:int,:int)<computes>:int => f[3,4]=f[4,5]}
test(S00){(1,2)=(1,2)}
test(F00){(1,2)=(1,3)}
test(F00){(3,2)=(1,2)}
test(S00){array{a:int} => array{b:int} => a=a}
test(D00){array{a:int} => array{b:int} => a=b}
test(D00){as:=array{:int} => as[0]=3}
test(S00){as:=array{:int} => bs:=array{:int} => as[0]=as[0]}
test(D00){as:=array{:int} => bs:=array{:int} => as[0]=bs[0]}

# Calling assumed function in its own context.
test(S00){(f(x:int)<computes>:int,a:=f(3)) => f(3)=a}
test(S00){f(:int)<computes>:int=>{f(3),f(3)}}
test(D00){f(:int):int=>{f(3)=f(3)}} 						#TODO:FUN-OUT-EQ

# Array matching.
test(D00){f(x:int,y:int):int => f[1,2]=f[1,2]}
test(D00){f(x:int,y:int):int => f(1,2)=f(1,2)}
test(D00){f(x:int,y:int)<computes>:int => f(1,2)=f(1,3)}
test(S00){f(x:int,y:int)<computes>:int => f(1,2)=f(1,2)}
test(D00){f(x:int,y:int)<computes>:int => f(1,2)=f(1,x) where x:=3}
test(S00){f(x:int,y:int)<computes>:int => f(1,2)=f(1,x) where x:=2}
test(S00){f(x:int,y:int)<computes>:int => f[1,2]=f[1,x] where x:=2}
test(D00){a&b:int => f(x:int,y:int)<computes>:int => f(a,b)=f(b,a)}
test(S00){a&b:int => f(x:int,y:int)<computes>:int => f(a,b)=f(a,b)}
test(S00){a&b:int => f(x:int,y:int)<computes>:int => f(a,b)=f(a,x) where x:=b}
test(S00){f(x:=x,y:=y)<computes>:type{z:=z} => f[1,2]=f[1,2]}
test(U00){f(x:=x,y:=y)<computes>:type{z:=z} => f[1,2]=f[1,3]}
test(D00){a&b&c&d:int => f(:int,:int)<computes>:any => {f(a,b)=f(c,d),a=c,b=d}}
test(S00){a&b&c&d:int => f(:int,:int)<computes>:any => if{f(a,b)=f(c,d),a=c,b=d}}

#
# Tests requiring support for calling multiple function heads.
#

# Function eta conversion - abstract.
test(F00){f()<computes>:=3 => g()<computes>:=4 => c:=f()=g()}
test(QS00){(f&g)(:int)<computes>:int => allow{h:=f=g} where f(0)=f(0)}
test(QD00){(f&g)(:int)<computes>:int => allow{h:=f=g} where f(0)=f(1)}
#test(S00){(f&g)(:int)<computes>:int => allow{h:=f=g} where g(0)=g(0)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where g(0)=g(1)}
#test(S00){(f&g)(:int)<computes>:int => allow{h:=f=g} where h(0)=h(0)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where h(0)=h(1)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where f(0)=h(0)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where f(0)=h(1)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where g(0)=h(0)}
#test(D00){(f&g)(:int)<computes>:int => allow{h:=f=g} where g(0)=h(1)}
#test(S00){(f&g)(:any)<computes>:any=>f(3)=4=>b:=g(3)=>f=g=>b=4}
#test(S00){(f&g)(:any)<computes>:any=>f(3)=4=>b:=g(3)=>f=g=>b+1=5}

# Function eta conversion - concrete.
test(F00){f()<computes>:=3,g()<computes>:=4,c:=f()=g()}
#test(R04){allow{f(:int)<computes>:=3,g(:int)<computes>:=4,h:=f=g} where f(0)=f(0)}
#test(R04){allow{f(:int)<computes>:=3,g(:int)<computes>:=4,h:=f=g} where g(0)=g(0)}
#test(R04){allow{f(:int)<computes>:=3,g(:int)<computes>:=4,h:=f=g} where h(0)=h(0)}
#test(F00){allow{f(:int)<computes>:=3,g(:int)<computes>:=4,h:=f=g} where f(0)=h(0)}
#test(F00){allow{f(:int)<computes>:=3,g(:int)<computes>:=4,h:=f=g} where g(0)=h(0)}

# Former verifier IsReady assertions.
test(S00){G(x:=0|1):=2; for{G[_]}}
test(S00){for{:function(3|5){6}}}
#!!!!test(S00){for(i->x:function(j:=3|5){j+1}){i,x}}
test(S00){G(x:=0|1):=x; Print(for{G[0]})}
test(S00){a->b:=function(2){3}}
test(S00){G(x:int where x=(7|8)):=9; for{G[7]}}
test(S00){G(x:int where x=(0|1)):=x; Print(for{G[0]})}

# Implied AST normalization.
#LA test(S00){x is 3; Print(x)}
#LA test(S00){f(x in int):=x; f(3)=3}

# Bad macro parameters.
#LA test(M02){array(){}}
#LA test(M02){for<a>(b)<c>{d}}

# Assert.
test(QS00){assert{1=1}}
test(QR01){assert{1=2}}

# Throw.
test(X00){if{Throw[123]}}
test(X00){if{expect<succeeds>{Throw[123]}}}
test(X00){f():=Throw(3)}
#test(S00){f()<throws_any>:=Throw(3)}
#test(S00){f()<throws_any>:=Throw(4)}
#test(S00){f(x:int)<throws_any>:=Throw(x)}
#test(S00){f()<throws_any>:={2=3,Throw(4)}}
#test(S00){f()<throws_any>:={Throw(4),2=3}}

# Functions and eta issues. Occurs check.
#test(U00){f():=f,g():=g,f()=g()}
#test(U00){f():=f,g():=g,f=g}
#test(U00){f():=1,g():=2,(f=g)()}

# Why the hell does this multiply execution time?!
# Dividing file into halves only speeds up 7%, so it's not notification overhead.
#for{1|2|3|4}

# Note that we have computed and nested tests.
#for(i:=0|1) do test(F00){i=3}
#for(i:=0|1) do test(S00){nat[i]}

# Abstract for.
#test(S00){f(xs:[]int):=for(x:xs){x+1},f(3,5,7)=(4,6,8)}
#test(S00){Concatenate(xs:[]any,ys:[]any):=for{xs?|ys?},Concatenate((1,2),(3,4))=(1,2,3,4)}
#test(S00){n:nat => for(x:0..n,y=:10|20){x+y}}                  # (x+10,x+20)+..
#test(S00){n:nat => for(y:=10|20,x:0..n){x+y}}                  # (x+10,..)+(x+20,..)

# Verifying all branches of conditions and choices.
#test(N00){if(1){2}else{x}}
#test(N00){if(1=2){x}else{3}}
#test(N00){if(1|x){2}}

# Abstract natives.
#test(S00){f(x:int):=x+1,f(3)=4}
#test(F00){f(x:int):=x+1,f(3)=5}
#test(S00){f(a:int,b:int where g:=function(){a=b}):=g(a,b)}
#test(S00){f(x:int):=-x,f(3)=-3}
#test(S00){f(x:int):=1+2,f(3)}
#test(S00){f(x:int):=x+10,7,f(3),8}
#test(S00){f(x:prefix'-'):=x+10,Print(f(3)),f(3)=7}
#test(S00){f(x:int):={a:=x,b:=a*x},f(1)}
#test(S00){function(x:int):=let(y:=2) do y,f(4)=2}
#test(S00){f(x:int,x):={}}
#test(S00){f(x:int):=let(y:=x*2) do x+y,f(4)=12}
#test(S00){f(g(:int):int):=g(3)=>f(operator'-')}
#test(U00){f(g(:int):int):=g(3)=>f(int=operator'-')}

# Verifier doesn't apply parent reductions locally in child contexts.
#test(U00){a:int => f(x:int):=x+1 => b:=f(a) => a=2 => b=3}        # Not automatically supported.
#test(S00){a:int => f(x:int):=x+1 => b:=f(a) => a=2 => {f(a),b=3}} # Supported when invoked explicitly.

# Variables.
#test(S00){x^:int=123,x^=123,x:=124,x^=124,x+=2,x=126}

# Variables and forks.
#test(S00){vs:=for(x:=1|2,y^:int=x){y},vs[0]<>vs[1]}
#test(S00){vs:=for(x^:int:=0,y:=1|2,x:=y){x},vs[0]<>vs[1]}

# Top-level forks falling back to wait-and-test.
#Rework based on DeriveFx test(A00){2=>3|4} # When we support no-fork a|b, I00 would be better error.
#test(S00){1=1|2}
#test(S00){2=1|2}
#test(F00){3=1|2}
#test(S00){function(x:false|int){x}[3]=3}
#test(S00){function(x:false|prefix'-'){x}[3]=-3}
test(S00){(3,5,7)[i],i:=1}
test(F00){(3,5,7)[i],i:=4}
test(X00){1|2} # Improve error to I00.
test(X00){function(1){2|3}} # Same.
#test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,3|4]} # Join where fx::Fork disallowed should fall back to test.
#test(S00){x:int => f():t=y where y:int => if(x=f[]){1}else{2},t(z:=1|2):=z} # Same.

# Array generation recursion.
#test(S00){Print of ns:=for(i:=0..9) do ns[i-1]+1 or 0}
#test(S00){Print of fs:=for(i:=0..9) do fs[i-1]+fs[i-2] or 1}
#test(S00){fs:=array{1,1}+for(i:=0..8) do fs(i)+fs(i+1)}

# Proofs as programs.
# Note that we can construct converges proofs in imperative code by building up converges functions.
