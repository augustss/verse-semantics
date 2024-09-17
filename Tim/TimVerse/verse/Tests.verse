
# This is stuck on the 'int', possibly because we don't have h computes.
#test(S00){(f&g)()<computes>:int => f[]=g[] => f[]=g[]}

#!! <succeeds><ambiguates> :: <abstracts>
#	InFunction=InLocalNative[InAbstraction_out]=InLocalNative[InAbstraction_out].abstraction_type[InAbstraction_out]=InAbstraction_out=InAbstraction_in=any=EnterFunction=InAbstraction_out=InAbstraction_in=InLocalNative[InAbstraction_out]=InFunction=InLocalNative[InAbstraction_out].abstraction_type[InAbstraction_out]=EnterFunction=InAbstraction_out=InAbstraction_in=InLocalNative[InAbstraction_out]=InFunction=InLocalNative[InAbstraction_out].abstraction_type[InAbstraction_out]=EnterFunction=InAbstraction_out=InAbstraction_in=InLocalNative[InAbstraction_out]=InFunction=InLocalNative[InAbstraction_out].abstraction_type[InAbstraction_out]=EnterFunction
#	ldp=DomainOutput=CallFunction_out[CallParameter_out]=CallFunction_out[CallParameter_out].f_lam[CallParameter_out]=EnterOutput=EnterFunction[ComposeOutput]=EnterFunction[ComposeOutput].any[ComposeOutput]=ComposeOutput=CallFunction_out[CallParameter_out]=CallFunction_out[CallParameter_out].g_lam[CallParameter_out]=EnterOutput=EnterFunction[ComposeOutput]=EnterFunction[ComposeOutput].any[ComposeOutput]=ComposeOutput=lrp=ComposeInput  !  CallFunction_out[CallParameter_out]  !  lrr=ComposeOutput  !  CallFunction_out[CallParameter_out].f_lam[CallParameter_out]  !  EnterOutput=EnterFunction[ComposeOutput]=EnterFunction[ComposeOutput].any[ComposeOutput]=ComposeOutput  !  CallFunction_out[CallParameter_out]  !  CallFunction_out[CallParameter_out].g_lam[CallParameter_out]  !  EnterOutput=EnterFunction[ComposeOutput]=EnterFunction[ComposeOutput].any[ComposeOutput]=ComposeOutput

<#
# Essentials.
test(S00){f():=4; f[]}
test(F00){f():=4; f[]=3}
test(F00){f():=4; f[]; f[]=3}
#test(D00){g(f():int)<decides>:={expect<succeeds>{f[]}; f[]=3}} # We need this to enable future memoization option.
#>

# Bidirectionality.
#f:=int; f[_]=3
#f:=int => f[_]=3

<#
# Pure function call equality.
test(D00){f(x:int):int           => f[3]=f[3]}
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(S00){f(x:any)<computes>:any => f[3]=f[3]}
test(D00){f(x:int)<computes>:int => f[3]=f[4]}
test(U00){f(x:int)<computes>:any => f[3]=f[4]}
test(S00){(f(:int)<computes>:int,f[3]=4) => f[3]=4}
test(S00){(f(:int)<computes>:int,f[3]=4) => f(3)=4}
test(F00){(f(:int)<computes>:int,f[3]=4) => f[3]=5}
test(F00){(f(:int)<computes>:int,f[3]=4) => f(3)=5}
test(U00){a&b:any => f(_    )<computes>:any => f[a]=f[b]}
test(U00){a&b:any => f(x:any)<computes>:=x  => f[a]=f[b]}
test(D00){a&b:int => f(x:int)<computes>:=x  => f[a]=f[b]}
test(S00){f(:any)<computes>:any              => f[2]=3  => f[2]=3}
test(S00){f(:any)<computes>:any              => f(2)=3  => f[2]=3}
test(F00){f(:any)<computes>:any              => f(2)=3  => f[2]=4}
test(U00){f(:any)<computes>:any              => f(2)=3  => f[3]=3}
test(U00){f(:any):any                        => f[2]=3  => f[2]=3} # Not <computes>
test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=as}
test(F00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=(_,_,_)}

# Collected slack introduced by new (ComputesCallEqual) via MatchingCallReps.
test(S00){(f&g)()<computes>:any => f[]=3 => f[]=3} # Works with MatchingCallReps, not without.
test(S00){(f&g)()<computes>:any => f[]=g[] => f[]=g[]}
test(S00){a&b:any => (f&g)(:any)<computes>:any => f[a]=g[b] => f[a]=g[b]}
test(S00){a:any => f(:any)<computes>:any => int[f[a]] => int[f[a]]}
test(S00){a:any => f(:any)<computes>:any => nat[f[a]] => int[f[a]]}
test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=(_,_)}
test(S00){f(:any)<computes>:any => f[2]=(_,_) => f[2]=(_,_)}
#>

#
# General fallout from ComposeContext.
#

# RunComposeStep collected loops.
#test(S00){a&b:int => {assume{x=a},x:=a=b}}
#test(S00){a&b:int => {assume{x=b},x:=a=b}}
#test(S00){a&b:any => {assume{x=a}; x:=a=b}}
#test(U00){a&b:any => {assume<decides>{x=a}; x:=a=b;}}
#test(S00){assume{x=(_,_,_)},x:=(_,_,_)}
#test(R20){assume{x=(_,_,_)},x:=(_,_)}
#test(S00){assume{x=3} where x:=3} # Loop.

# Runtime: Why does the later succeed without printing?
#test(S00){Print("Hi")}
#test(S00){for(3|5) do Print("Hi")}
#test(S00){f()<interacts>:=Print(456); f[]}

# Other.
#test(S00){f(x:=first{2=3}):=x=1}
#test(S00){x:=first{:false} => x=0}
#test(S00){x:=(not 3) => x=0}
#test(S00){x:=not 3 => x=0}

# Simple result of 'expect' setting allow?
#test(D00){f(x:int)<decides>:=expect<succeeds>{    :type{x=3}          } => 4} # Result of 'expect' setting allow.

#
# Various fallout.
#

# The overload nonmonotonicity problem exists with tuples too.
# Can't tell if this is because we go straight to succeeds|fails or if we go resolves->succeeds|fails.
#test(F00){x:=2; if{x=2}; x=3}                            # resolves->fails
#test(F00){x:=2; if{x=3}; x=3}                            # resolves->fails
#test(F00){x:=2=3; if{x=2}}                               # straight to fails
#test(F00){xs:=array{8}; if{xs[0]}; xs=array{}}           # straight to succeeds
#test(F00){xs:=array{}; if{xs[0]}; xs=array{8}}           # straight to fails
#test(F00){xs:=array{}=array{8}; if{xs[0]}}               # straight to fails
#test(F00){f:=function(){8}=function(){7}; if{f[]=8}}     # straight to fails
#test(F00){f:=function(){8}; if{f[]=8}; f=function(){7}}  # loop
#test(F00){f:=function(){8}; if{f[]=7}; f=function(){7}}  # loop

# Is decides-forking a general solution to the unifies-incomplete overload problem?
#test(S00){x:=8; if{x=8}} # decides-forks because we can't succeed while there are unifies-incomplete dependencies
#test(S00){xs:=array{8}; if{xs=array{8}}} # decides-forks
#test(S00){xs:=array{8}; if{xs[0]=8}} # decides-forks
#test(S00){xs:=array{8}; if{xs[0]}} # PROBLEM: doesn't decides-fork
#f:=function(6){7}; if{f[6]} # PROBLEM: doesn't decides-fork

#f:=(1=>2)=(1=>2)
#f:=(1=>2); f=(1=>2) # loops. unsure why.
#f:=(1=>2),f=(1=>2) # loops, unsure why.

# Stage.
#f(x:int):int=3; y:=f[y] # Yikes, should reject.
#F(i:int)<decides>:={f(p:nat):nat=p; r:=f[i]; expect<succeeds>{nat[r]}}

# Do we need to (and can we soundly) flow domain membership like dominators?
#test(S00){f(a:int)<decides>:={b:=nat[a]; expect<succeeds>{nat[b]}}} # D00
#test(S00){f(a&b:int    )<decides>:={c:=a=b; expect<succeeds>{int[c]}}} # D00
#test(D00){f(a:int,b:nat)<decides>:={c:=a=b; expect<succeeds>{nat[c]}}}
#test(D00){f(a&c:int)<decides>:={b:=a=c; expect<succeeds>{b=c}}}
#test(D00){f(a&c:int)<decides>:={t:=type{c}; b:=t[a]; expect<succeeds>{t[b]}}}
#f(a:int)<decides>:={t:=type{3}; b:=t[a]; expect<succeeds>{t[b]}} # Works, WTF?
#f(a:int)<decides>:={t:=type{3}; b:=t[a]; expect<succeeds>{b=3}}
#f(a:int)<decides>:={t(x:=3):=x; b:=t[a]; expect<succeeds>{b=3}}
#f(a:int)<decides>:={b:=function(x:=3){x}[a]; expect<succeeds>{b=3}}

# Misc.
#test(S00){t(a:any):=a => function(g(1):t[any]){g}[h(:t[any]):=2][1]=3}

# Fix.
test(S00){nat=nat}
#test(U00){nat=nat<succeeds>}
#test(U00){nat<succeeds>=nat<decides>}
#test(S00){nat<succeeds>=nat<succeeds>}

#
# NEW_FUNCTION_FLOW lambda-only breakage.
#
#test(S00){f(1):=2 => f[1]=2}
#test(S00){f(x:any):=x => f[7]=7}
#test(S00){f(x:any):=x => f[_]=3}
#test(S00){f:any => f=any => f[_]=3}
#test(S00){f(1=>2,3):=7 => f[1=>2,3]}
#test(S00){f():nat => int[f[]]}
#test(V10){a:int => :int=a=3}
test(D00){f(:int):int => f[3]=f[4]} # Breaks if we remove dead-end dominators.
test(D00){f(:int):int => f[3]=f[3]} # Breaks if we remove dead-end dominators.
#test(D00){f(:int):int => f(3)=f(4)} # Breaks regardless.
#test(D00){f(:int):int => f(3)=f(3)} # Breaks regardless.

#
# NEW_FUNCTION_FLOW presumed unification-removal breakage:
#
test(S00){int[3]}
test(F00){int['x']}
test(S00){f(1):=2; f[1]}
test(S00){f(1):=2; f[1]=2}
test(F00){f(1):=2; f[1]=3}
test(S00){f(1):=2 => f[1]}
test(F00){f(1):=2 => f[1]=3}
test(S00){-0}
test(S00){x:=int[x]=123}
test(S00){int[_]=123}
test(S00){x:int; x=3}

#test(F00){function(g(1):abstraction_type[any]){g}[h(:abstraction_type[any]):=2][1]=3}
test(F00){function(g(1):any){g}[h(:any):=2][1]=3}
test(F00){function(g(1):int){g}[h(:int):=2][1]=3}

# Basic stage breakage.
#test(S00){if{x:int<fails>:=false?}}
#test(S00){if{x:int<fails>:=3}}
#
#test(S00){f(x:int):int => if{f[4]=4}}
#test(S00){f(x:int):int=x; if{f[4]=4}}
#test(R01){f(x:int):int=x; if{f[4]=5}}
#
#test(S00){x:int=let(x) do 3}
#test(S00){xs:type{3,:int}=let(xs) do (3,5)}
#test(S00){xs:type{3,:int}=(xs[0],7)}
#test(S00){xs:type{3,:int}=(xs[0],xs[0])}
#
#test(S00){f(x:int):false=f[x]}

# Stage.
#test(S00){for{x:int<succeeds>=3}}
#test(A00){for{x:int=3}}

# Native type_fx_abstraction fixes the disabled tests:
#test(S00){f(:int):int<computes>=3; f[3]=f[3]}

#f():int<succeeds> => f
#x:int<succeeds>=3

# Higher order unification of non-<succeeds> lambdas.
#test(S00){function()<fails>{false?}}
#test(U00){function()<fails>{:int}=function(){4}}
#test(U00){function()<fails>{:int}=function(){'c'}}
#test(S00){function()<fails>{:int}=function<fails>(:false)}
#test(S00){function()<fails>{:int}=false}
#test(S00){function()<decides>{2}}
#test(S00){function()<decides>{:false}}
#test(U00){function()<decides>{int=nat}}
#test(U00){function()<decides>{:int}}
#test(U00){function()<decides>{:int}=function()<decides>{:int}}
#test(S00){function()<decides>{:int}=function(){2}}
#test(S00){function()<decides>{:int}=function()<fails>{:false}}
#test(S00){function()<decides>{:int}=false}
#test(U00){function()<decides>{:int}=function<resolves>{int=nat}}
#test(S00){function()<resolves>{int=nat}}

# New flaw: we propagate even <rejects> through ':t'.
#test(C00){f():3 => 4}

# 'if' is stuck on eval-future. Must be because verify <succeeds>, beta <decides>.
#test(S00){f(x:=2,if{Hmm:=x=2}):=3; f[2,2]}

# Conditions Triggering Refork breakage.
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=10 where B:=20}
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=11 where B:=20}
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=10 where B:=21}

# Lennart: Verifier accepts this unsoundly...
#test(S00){F(r:int)<succeeds>:={x:int; if(r=5) then x=1 else x=2; if(r=5) then x=1 else x=3}}

#
# Simple scalar tests.
#

# Simple constants.
test(S00){1}
test(S00){'x'}
test(S00){false}
test(S00){()}

# Simple unification.
test(S00){3=3}
test(F00){3=5}
test(S00){'3'='3'}
test(F00){'3'='4'}
test(F00){3='3'}
test(S00){()=()}
test(F00){3=()}

# Simple definitions.
test(S00){x:=1}
test(S00){y:=2 where y=2}
test(S00){x:=1; x=1}
test(F00){x:=1; x=2}
test(S00){x=1; x:=1}
test(F00){x=2; x:=1}
test(A00){x:=x}
test(S00){x:=x; x=3}
test(A00){x:=y; y:=x}
test(S00){x:=y; y:=x; x=3}
test(S00){x:=y; y:=x; y=3}
test(S00){x:=1; y:=1; x=y}
test(S00){x=y; x:=1; y:=1}
test(F00){x:=1; y:=2; x=y}
test(F00){x=y; x:=1; y:=2}
test(S00){x:=123=x}
test(S00){x:=x=123}

# Simple identifier not found.
test(N00){x}
test(N00){x{}}

# Simple duplicate identifier.
test(N04){x:=1; x:=1}
test(N04){x:=0; x:=1}
test(N04){x:=0; x:int => 3}
test(N04){x:=0; y:int => x:=1}

# Simple sequences.
test(S00){1; 2}
test(F00){1=2; 3}
test(F00){1; 2=3}
test(S00){x:=1; x}
test(S00){x:=1; x=1}
test(F00){x:=1; x=2}
test(S00){x; x:=1}
test(S00){x=1; x:=1}
test(F00){x=2; x:=1}

# Simple placeholder.
test(A00){_}
test(S00){_=3}
test(S00){3=_}
test(S00){_:=3}
test(S00){_=3=_}
test(F00){3=_=4}

# Simple 1-ary natives.
test(S00){-3}
test(F00){-3=3}
test(S00){--3=3}
test(S00){int[123]}
test(S00){int(123)}
test(S00){int[123],int(123)}
test(S00){f:=prefix'-',p:=3,f(p)}
test(S00){(f:=prefix'-')(p:=3),f,p}
test(F00){false[0]}
test(F00){:false}
test(S00){int[0]}
test(F00){int['x']}
test(F00){3 => {4=5}}
test(S00){void()=array{}}
test(S00){if{2=3}=false}
test(S00){int[_]=123}
test(S00){x:=int[x]=123}
test(S00){-x,x:=1}

# Simple native types.
test(S00){int}
test(S00){int[3]}
test(F00){int['3']}
test(F00){int['x']}
test(A00){:int}
test(A00){x:int}
test(S00){x:int; x=3}
test(S00){x:int; x=-3}
test(F00){x:int; x='3'}
test(S00){x:nat; x=3}
test(F00){x:nat; x=-3}
test(F00){x:nat; x='3'}
test(F00){x:int; x='x'}
test(A00){:any}
test(A00){a:any}
test(S00){a:any; a=3}

#
# Simple tuples and arrays.
#

# Simple.
test(S00){1}
test(S00){1,2}
test(S00){array{}}
test(S00){array{1,2}}
test(S00){y:=2,y=2}
test(S00){y:=2,y}
test(S00){y,y:=2}
test(S00){2=2,3=3}
test(F00){2=2,3=4}
test(F00){2=3,4=4}
test(A00){array{_}}
test(S00){(1,2) where (1,2)}

# Simple tuple unification.
test(S00){()=()}
test(F00){array{}=array{3}}
test(S00){array{3}=array{3}}
test(F00){array{3}=array{4}}
test(F00){(10,20)=(10,20,30)}
test(S00){as:=array{3}=array{_}}
test(A00){as:=array{_}=array{_}}
test(F00){as:=array{3}=array{4}}
test(S00){array{1,2}=array{1,2}}
test(F00){array{1,1}=array{1,2}}
test(F00){array{1,2}=array{1,3}}
test(F00){array{1,1}=array{2,1}}
test(F00){array{2,1}=array{3,1}}
test(F00){array{1,2}=array{2,1}}
test(F00){array{1,2}=array{3,4}}
test(S00){array{1,2}=array{_,_}}
test(S00){array{_,_}=array{3,4}}
test(S00){array{1,_}=array{_,4}}
test(S00){array{_,2}=array{3,_}}
test(F00){array{1,2}=array{3,_}}
test(F00){array{_,2}=array{3,4}}
test(F00){(_,_)=(_,_,_)}
test(S00){x=array{20},x:=array{20}}
test(F00){x:=2  where array{x}=array{3}}
test(F00){x:int where array{x,2}=array{3,x}}
test(F00){x:=x  where array{x,2}=array{3,x}}

# Array element access.
test(A00){array{_}[0]}
test(S00){array{3}[0]}
test(S00){array{_}[0]=4}
test(S00){array{3}[0]=3}
test(F00){array{3}[0]=4}
test(S00){array{2,3,4}[1]=3}
test(F00){array{2,3,4}[5]}
test(S00){array{a:=a}[0]=9}
test(A00){array{a:=a,b:=b}[1]=9}

# Tuple unification vs other types.
test(F00){3=(4,5)}

# Element access.
test(S00){as:=array{2,3,4}; as[1]}
test(S00){as:=array{2,3,4}; as[1]=3}
test(S00){as:=array{3}; as[0]=3}
test(S00){as:=array{2,3,4}; as[1]=3}
test(S00){array{2,3,4}[1]=3}
test(S00){as:=array{3}; as[0]=3}
test(S00){as:=(3,5); as[0]=3}
test(F00){as:=(3,5); as[0]=4}
test(F00){as:=(3,5); as[0]=5}

# Simple natives taking tuples.
test(S00){1+2}
test(S00){1+2=3}
test(F00){1+2=4}
test(S00){2*3=6}
test(F00){2*3=7}
test(S00){if(1+1) then 2+1 else 3+1}
test(S00){operator'+'[1,2]}
test(S00){1/2}
test(F00){1/0}
test(F00){1+2=5}
test(S00){Length(1,2)=2}
test(S00){x+2,x:=1}

# Arrays with cyclic nesting.
test(A00){as:=(1,as)}
test(A00){as:=(1,(2,as))}
test(A00){as:=(1,(as,2))}
test(A00){as:=array{as}}
test(A00){as:=(1,as),bs:=(1,bs)}
test(A00){as:=(1,as),bs:=(1,bs),as=bs}
test(F00){as:=(1,as=2)}
test(U00){a:any => as:=(1,as=a)}

# Arrays with precisely located failure.
test(U00){F(as&bs:=array{_}                   )<resolves>:={cs:=as=bs                           where expect<succeeds>{as[0]=bs[0]}}}
test(S00){F(as&bs:=array{_}                   )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
test(S00){F(as&bs:=array{_}                   )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]=cs[0]}}}
test(S00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
test(S00){F(as&bs:=array{:int}                )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
test(S00){F(as:=array{:int})(bs:=array{:int}  )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
test(S00){F(as:=(:int,:int))(bs:=(:int,:int)  )<resolves>:={xs:=as=bs                           where expect<succeeds>{xs[0]}}}
test(S00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]=cs[0]}}}
test(S00){F(as:=array{:int})(bs:=array{:int}  )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
test(S00){F(as:=array{:int})(bs:=array{:int}  )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]=cs[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{bs[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{cs[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]=as[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{bs[0]=bs[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{cs[0]=cs[0]}}}
test(S00){F(a:int                             )<resolves>:={as:=array{a}=array{5}               where expect<succeeds>{as[0]}}}
test(S00){F(a&b:int                           )<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]}}}
test(S00){F(as&bs:=array{array{_}}            )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0][0]}}}
test(S00){F(as&bs:=array{array{_}}            )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0][0]=cs[0][0]}}}
test(S00){F(as&bs:=array{array{_}}            )<resolves>:={cs:=as[0]=bs[0]                     where expect<succeeds>{cs[0]}}}
test(S00){F(a&b:any                           )<resolves>:={cs:=array{a}=array{b}               where expect<succeeds>{cs[0]}}}
test(S00){F(as&bs:=array{_}                   )<resolves>:={cs:=as=bs                           where expect<succeeds>{cs[0]}}}
#
test(S00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{as[0]}}
test(S00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{bs[0]}}
test(S00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{as[0]=as[0]}}
test(S00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{bs[0]=bs[0]}}
test(D00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{as[0]=cs[0]}}
test(D00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{bs[0]=cs[0]}}
test(D00){F(as:=array{:int})(bs:=array{:int})<resolves>:=cs:=as=bs where expect<succeeds>{as[0]=bs[0]}}
#
test(S00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{a}}}
test(S00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{b}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]=cs[0]}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{bs[0]=cs[0]}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]=bs[0]}}}

#
# Simple function tests.
#

# Simple functions.
test(S00){1=>2}
test(S00){f(1):=2}
test(S00){function(){}}
test(S00){function(1){2}}
test(S00){function(x:=1){x=1}}
test(F00){function(x:=1){x=2}}
test(F00){function(){1=2}}
test(F00){function(){:false}}
test(S00){function(1){1; int[2]; 3}}
test(S00){function(x:=x){x}}
test(S00){function(x:=x){x=x}}
test(S00){function(x:=x,y:=x){x=y}}
test(U00){function(x:=x){x=1}}
test(U00){function(x:=x,y:=y){x=y}}

# Functions nested in range.
test(S00){1 => 2 => 3}
test(S00){(1 => 2) => 3}
test(S00){f(x:int):=1 => 3}

# Functions with arrays in domain.
test(S00){a:=(3,4) => a}
test(S00){a:=(3,4) => (3,4)}
test(F00){a:=(3,4,5) => a=(3,5)}
test(F00){a:=(3,4) => a=(3,5)}

# Simple calls.
test(F00){f:=function(1){2}; f[3]}
test(S00){f(1):=2 => f[1]}
test(F00){f(1):=2 => f[3]}
test(P00){f(1):=2 => f(3)}
test(S00){f(1):=2 => f[1]=2}
test(F00){f(1):=2 => f[1]=3}
test(F00){f(1):=2 => f(1)=3}
test(F00){f(x:=7):=x => f[9]}

# Functions with types and abstractions in domain.
test(S00){x:any => x}
test(S00){x:any => x=x}
test(U00){x:any => x=3}
test(D00){x:int => x=3}
test(U00){x:any => y:any => x=y}
test(D00){x:int => y:int => x=y}
test(S00){(x:int => x)[2]=2}
test(S00){a:int => {x:=x,x=a}}
test(S00){a:int => {x:=x,a=x}}
test(S00){a:int => {x:=x,x=a,a=x}}
test(D00){a&b:int => {x:=x,x=a,x=b}}
test(S00){x&y:int => x=y => 3}
test(S00){x&y:int => (x=y,2) => 3}
test(S00){x&y:int => (2,x=y) => 3}
test(S00){x&y:int => (x=y where f():=x=y) => 3}
test(S00){x&y:int => (f():=x=y,f[]) => 3}
test(S00){g(f(3):=4):=5; g[f(2):=4] => 5}
test(S00){x&y:int => (f():=x=y,f[]) => 3}
test(S00){f(x:prefix'-'):=x,f(3)=-3}
test(S00){f(x:=(:prefix'-')=(:prefix'-')):=x,f(3)=-3}
test(P00){f(x:=(:prefix'-')=(:int)):=x,f(3)}
test(S00){f(x:=(:prefix'-')=(:int)):=x,f(0)}

# Functions with types in domain, and calls.
test(S00){f:=function(x:int){x}; f[1]}
test(S00){f:=function(x:int){x}; f(1)}
test(S00){f:=function(x:=int[x]){x}; f(1)}
test(S00){f(x:int):=x; f[3]=3}
test(D00){f()<decides>:int => f[]}
test(S00){f():=3 => f[]=3}
test(F00){f():=3 => f[]=4}
test(U00){f():any => f[]='3'}
test(D00){f():int => f[]=3}
test(D00){f():comparable => f[]='3'}
test(F00){f():int => f[]='3'}
test(S00){f(x:int):=x => f[7]}
test(S00){f(x:int):=x => f[7]=7}
test(S00){f(x:int):int => f[7]}
test(D00){f(x:int):int => f[7]=7}
test(S00){(x:int => x)[2]=2}
test(S00){f(x:int):=x; f[3]+1=4}
test(S00){f(x:int):=x; f[3]=3}
test(F00){f(x:int):=x; f[3]=4}
test(S00){f(x:int,y:int):=0 => f(3,5)}
test(S00){a:int => {x:int,x=a}}
test(D00){a:int => {x:nat,x=a}}
test(S00){t(x:int):=x; f(x:t):=t[x]}
test(U00){t(x:int):=x; f(x:any):=t[x]}

# Parameter checking going badly.
test(F00){f(x:any):=x  => f(3)=5}
test(F00){f(x:=3 ):=x => f(3)=5}
#test(F00){f(x:int):=x => f(3)=5}            # Prefer F00.
#test(F00){f(x:int):=x => f(3)='5'}          # Prefer F00.
test(F00){as:=array{3} => as(0)=4}           # Changes to P00 if WhenCastStep considers deep effects.
test(P00){f(x&y:int):=(y,x) => f(3,5)=(5,5)} # Should be F00.

# Functions with arrays and types in domains.
test(D00){a:=(:int,:int) => a=(3,4)}

# Functions with choice in domain.
test(S00){function(x:=1|2){x}}
test(S00){function(x:=0|1){-x}}
test(S00){function(x:=0|1){let(x){}}}
test(F00){function(x:=1|2){x=1}}
test(F00){function(x:=1|2){x=2}}
test(F00){function(x:=1|2){x=3}}
test(S00){function(x:=1|2){int[x]}}
test(F00){function(x:=3|5){x+1=4}}
test(F00){function(x:=3|5){x+1=6}}

# Anonymous functions.
test(S00){_(x:int):=int[x]}
test(D00){_(x:int):=nat[x]}

# Calls to non-functions.
test(C00){1[2]}
test(C00){1(2)}
test(C00){1[2] => 3}
test(C00){1(2) => 3}
test(C00){f:any => f[2] => 3}
test(C00){f:any => f(2) => 3}

# Calls with bad parameters.
test(F00){false[2]}
test(P00){false(2)}
test(S00){f(:int):int => f(2) => 3}
test(P00){f(:int):int => f() => 3}
test(S00){f:=(1,2,3) => f[2] => 3}
test(S00){f(:int):int => f[2] => 3}
test(S00){(f(2):=1,f[2]) => 3}

# Functions and calling with dependency.
test(S00){f(x:int,x):=x; f[3,3]}
test(F00){f(x:int,x):=x; f[3,4]}

# Function scoping.
test(N00){function(1){x}}
test(N00){function(x){1}}
test(S00){function(x:=1){x}}
test(N00){function(x){x:=1}}
test(S00){function(1 where a:=2){a}[1]=2}
test(F00){function(1 where a:=2){a}[1]=3}

# Simple bidirectional calls.
test(S00){f(x:any):=x      => f[3]=3}
test(F00){f(x:any):=x      => f[3]=4}
test(S00){f(x:any):=x      => f[3]=_}
test(S00){f(x:any):=x      => f[_]=4}
test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]}
test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=3}
test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=3}
test(F00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=4}
test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][3]=_}
test(S00){f(g(:int):int)(x:int):=g[x] => f[h(x:int):=x][_]=3}
test(U00){f(x:any):type{x} => f[_]=4}

#
# Higher order functions.
#

# Higher order function definitions.
test(S00){f(x:int):=1 => f[0]}
test(S00){f(x:int):=1 => f[0]=1}
test(F00){f(x:int):=1 => f[0]=2}
test(D00){a:int => f(x:int):=1 => f[0]=a}
test(S00){f(x:int):=1 => f[0]=f[0]}
test(S00){f(x:int):int => f[0]}
test(S00){f(x:int):int => int[f[0]]}
test(D00){f(x:int):int => nat[f[0]]}
test(D00){f(x:int):int => f[0]=3}
test(D00){f(x:int):int => f[0]=f[0]}

# Implied higher order functions are still effects-checked.
test(D00){x&y:int => f():=x=y => 3}

# Higher order function calling.
test(S00){f(g(1):=1):=2; f[h(1):=1]}
test(U00){f(g(1):=1):=2; f[h(2):=1]}
test(U00){f(g(1):=1):=2; f[h(1):=2]}
test(S00){f(g(1):=1):=2; f[h(:int):=1]}
test(S00){f(g(1):int):=2; f[h(:int):=1]}
test(U00){f(g(1):=1):=2; f[h(:int):=2]}
test(U00){function(g(1):=2     ){g}[h(0    ):=2]}
test(F00){function(g(1):=2     ){g}[h(0    ):=2][1]}
test(S00){function(g(1):=2     ){g}[h(1    ):=2]}
test(S00){function(g(1):=2     ){g}[h(1    ):=2][1]=2}
test(S00){function(g(1):=2     ){g}[h(x:int):=2][1]=2}
test(F00){function(g(1):=2     ){g}[h(x:int):=2][1]=3}
test(F00){function(g(1):=3     ){g}[h(x:int):=2][1]}
test(S00){function(g(1):int    ){g}[h( :int):=2][1]=2}
test(F00){function(g(1):int    ){g}[h( :int):=2][1]=3}
test(F00){function(g(x:int):int){g}[h( :any):=2][]}

# Higher order functions with arrays.
test(S00){f(1=>2):=7 => f[1=>2]}
test(S00){f(1=>2,3):=7 => f[1=>2,3]}
test(S00){f(g(:int):int,3):=7 => f[any,3]}

# Higher order functions with types.
test(S00){f(g(:int):int):=g[3] => f[int]}

# Function use of visible assumption to check inputs.
test(D00){a&b:int => f(g(4          ):=5):=6 => f[h(4          ):=5] where a=b}
test(S00){a&b:int => f(g(4 where a=b):=5):=6 => f[h(4          ):=5]}
test(U00){a&b:int => f(g(4          ):=5):=6 => f[h(4 where a=b):=5]}
test(D00){a&b:int => f(g(4 where a=b):=5):=6 => f[h(4          ):=5] where a=b}
test(U00){a&b:int => f(g(4          ):=5):=6 => f[h(4 where a=b):=5] where a=b}
test(S00){a&b:int => f(g(a=b        ):=5):=6 => f[h(a=b        ):=5]}

# Function domain plumbing in general.
test(S00){function(1){2}[1]}
test(F00){function(1){2}[0]}
test(S00){function(x:prefix'-'){x}(3)=-3}
test(S00){function(x:=-3){x}[-3]}
test(F00){function(-3){4}[3]}
test(S00){function(let(1){2}){4}[2]}
test(F00){function(let(1){2}){4}[3]}
test(S00){a:=2,function(a){3}[2]=3}
test(S00){function(x:int,y:prefix'-'){x,y}[3,4]=(3,-4)}

# Implied higher order functions.
test(S00){f(g(:int):int):=g[3] => f}
test(S00){f(g(:int):int):=g[3] => f[int]}
test(S00){f(g(:int):int):=g[3] => f[h(x:int):=x]}
test(S00){f(g(:int):int):=g[3] => f[h(x:int):=x]=3}
test(F00){f(g(:int):int):=g[3] => f[h(x:int):=x]=4}

# Higher order types.
test(S00){f():=int => f()[3]}
test(F00){f():=int => f()['3']}
test(F00){f():=int => f()[3]=f()[4]}

#
# Type.
#

# Simple.
test(S00){type{3}}
test(S00){type{:int}}
test(N00){type{x}}
test(S00){type{3}[3]}
test(F00){type{3}[4]}
test(S00){type{:int}[3]}
test(S00){type{:int}[4]}
test(F00){type{:int}['c']}
test(S00){type{:prefix'-'}[3]}
test(S00){type{:prefix'-'}[3]=-3}
test(F00){type{:prefix'-'}[3]=3}
test(S00){type{3,4}[3,4]}
test(F00){type{3,4}[3,5]}

# Type with dependency.
test(S00){t:=type{x:int,x},t[3,3]}
test(F00){t:=type{x:int,x},t[3,4]}
test(S00){t:=type{x:int,x},t[3,_]}
test(S00){t:=type{x:int,x},t[_,3]}
test(A00){t:=type{x:int,x},t[_,_]}

# Types and function resolves.
test(S00){f():=type{123} => 456}
test(S00){f():type{123} => f[]}
test(S00){f()():int => 2}
test(S00){f():type{g():int} => 2}

#
# Arrays and functions together.
#

# Multi-context array unification.
test(S00){as:=array{1} => 2}
test(S00){as:=array{_} => 2}
test(S00){as:=array{1} => as}
test(S00){as:=array{1} => as=array{1}}
test(F00){as:=array{1} => as=array{2}}
test(S00){as:=array{1} => as[0]}
test(S00){as:=array{1} => as[0] where as[0]}
test(S00){as:=array{1} => as[0]=1}
test(F00){as:=array{1} => as[0]=2}
test(S00){as:=array{1} => bs:=array{1} => as=bs}
test(F00){as:=array{1} => bs:=array{2} => as=bs}
test(S00){as:=array{:int} => as}
test(S00){as:=array{:int} => as=as}
test(S00){as:=array{:int} => as=array{:int}}
test(D00){as:=array{:int} => as=array{:nat}}
test(U00){as:=array{:any} => as=array{:nat}}
test(S00){as:=(:int,:int) => as=(as(0),as(1))}
test(D00){as:=(:int,:int) => as=(as(0),as(0))}
test(F00){as:=(:int,3) => as=(_,4)}
test(D00){a:int => (10,20)=(10,a)}
test(U00){a:any => (10,20)=(10,a)}
test(F00){f(x:int):=array{x,2}=array{3,x}}
test(D00){f(x:int,y:int):=array{x,2}=array{3,y}}
test(U00){f(x:int,y:any):=array{x,2}=array{3,y}}
test(S00){array{a:int} => array{b:int} => a=a}
test(D00){array{a:int} => array{b:int} => a=b}
test(D00){as:=array{:int} => as[0]=3}
test(S00){as:=array{:int} => bs:=array{:int} => as[0]=as[0]}
test(D00){as:=array{:int} => bs:=array{:int} => as[0]=bs[0]}
test(F00){f[array{5}]=5 where f(array{5}):=3}

# Functions returning arrays.
test(S00){f():=array{:any} => 4}
test(S00){f():=array{:any} => f()=array{_}}
test(S00){f():=array{:any} => f()[0]}
test(U00){f():=array{:any} => f()[0]=3}
test(S00){f():=array{:int} => 4}
test(S00){f():=array{:int} => f()=array{_}}
test(S00){f():=array{:int} => f()[0]}
test(D00){f():=array{:int} => f()[0]=3}
test(S00){f():=(3,:int) => 4}

#
# Simple 'if'.
#

# Known conditions.
test(S00){if{1}}
test(S00){if(1){2}else{3}}
test(S00){if(1=2){2}else{3}}
test(S00){if(1){2}else{false?}}
test(S00){if(1=2){false?}else{3}}
test(S00){2=if(1){2}else{3}}
test(F00){3=if(1){2}else{3}}
test(F00){2=if(1=2){2}else{3}}
test(S00){3=if(1=2){2}else{3}}
test(S00){if(1){2}else{3}=2}
test(F00){if(1){2}else{3}=3}
test(F00){if(1=2){3}else{4}=3}
test(S00){if(1=2){3}else{4}=4}
test(S00){if(1=2){3}else if(3){4} else{5}=4}
test(S00){if(1=2){3}else if(3=4){5} else{6}=6}
test(S00){if{1}=1}
test(S00){if{2=3}}
test(S00){if{123}=123}
test(F00){if{123}=456}
test(S00){if(x:=1){x=1}}
test(F00){if(x:=1){x=2}}
test(S00){if(x:=1){y:=2; y=2}}
test(S00){x:=1,if(x=1){2}else{3}=2}
test(F00){x:=1,if(x=1){2}else{3}=3}
test(S00){x:=1,if(x=2){2}else{3}=3}
test(F00){x:=1,if(x=2){2}else{3}=2}
test(S00){if(x=1){2}else{3}=2,x:=1}
test(F00){if(x=1){2}else{3}=3,x:=1}
test(S00){if(x=2){2}else{3}=3,x:=1}
test(F00){if(x=2){2}else{3}=2,x:=1}

# Known conditions; failure in iteration context after succeeds and commits.
test(F00){for{400|200}=(401,200)}
test(F00){function(100 where for{400|200}){300}[100]=400}
test(F00){function(100 where for(400|200){500}){300}[100]=400}
test(F00){function(for(x:=3|4){x}){8}[5,4]}

# Known conditions nested in functions.
test(F00){function(a:=if(1=2){3}else{4}){a}[3]}
test(S00){function(a:=if(1=2){3}else{4}){a}[4]}
test(S00){function(a:=if(1=1){3}else{4}){a}[3]}
test(F00){function(a:=if(1=1){3}else{4}){a}[4]}

# Timing.
test(S00){x:=1; if(x=1){3}else{4}}
test(S00){x:=1; if(x=2){3}else{4}}
test(S00){x:=1; if(x=2){3}else{4}=4}
test(F00){x:=1; if(x=2){3}else{4}=5}
test(S00){if(x=1){3}else{4}; x:=1}
test(S00){if(x=2){3}else{4}; x:=1}
test(S00){if(x=2){3}else{4}=4; x:=1}
test(F00){if(x=2){3}else{4}=5; x:=1}
test(S00){if(x=1,x:=(1|2)){4}else{5}=4}
test(S00){if(x=2,x:=(1|2)){4}else{5}=4}
test(S00){if(x=3,x:=(1|2)){4}else{5}=5}

# Scope.
test(S00){if(x:=1){x}}
#test(N00){if(x:=1){1}else{x}} # This currently succeeds because verifier isn't exploring not-taken control flow paths.
test(N00){if(x:=1){1}; x}
test(N00){if(1=1){x:=1}; x}
test(N00){if(1=2){x:=1}; x}
test(N00){if(1=1){1}else{y:=1}; y}
test(N00){if(1=2){1}else{y:=1}; y}

# Nested choice in 'if'.
test(S00){if((1|2)=1){4}else{5}=4}
test(S00){if((1|2)=2){4}else{5}=4}
test(S00){if((1|2)=3){4}else{5}=5}

# Nested choice in 'if' with symbol resolution.
test(S00){if(x:=1|2,x=1){4}else{5}=4}
test(S00){if(x:=1|2,x=2){4}else{5}=4}
test(S00){if(x:=1|2,x=3){4}else{5}=5}

# Conditions and arrays with precisely located failure.
test(S00){F(as&bs:=array{_})<resolves>:={cs:=as=bs where expect<succeeds>{if{cs[0]}}}}
test(S00){F(as&bs:=array{_})<resolves>:={cs:=as=bs where expect<succeeds>{if{ds:=cs where if{ds[0]}}}}}

#
# Functions: open and closed.
# NOTE: The distinction isn't currently implemented. Rather, higher order functions accept inputs functions as if they're open,
# but then existentially (instead of universally) quantify over their domain variables.
#

# Function <closed> flow cases:
test(S00){f(x:int)<closed>:=x; f[3]}                           # E0 parameter->domain->range->result
test(S00){f(x:int)<closed>:=x; f[_]=3}                         # E1 result->range->domain->parameter
test(S00){f(x:int,y:int)<closed>:=(x,y); f[3,_]=(_,4)}         # E2 mixed but independent
test(S00){f(x:int,y:int)<closed>:=(x,y,y); f[z,_]=(_,4,z:any)} # E3 mixed and dependent
test(S00){f(x:=1|2)<closed>:=x; a:int=>if{f[a]}}               # E4 forking and <decides>

#
# Simple 'for' and choice a|b with statically discoverable iteration.
#

# Basic for tests.
test(S00){for{}}
test(S00){for{}=array{array{}}}
test(F00){for{}=array{}}
test(F00){for{}=1}
test(S00){for{1}}
test(S00){for{}=for{}}
test(S00){for{1}=array{1}}
test(F00){for{1}=array{2}}
test(F00){for{1}=array{}}
test(S00){for{2=2}=array{2}}
test(F00){for{2=2}=array{3}}
test(S00){for{1=2}=array{}}
test(S00){for{1|2}=array{1,2}}
test(S00){for{1|2|3}=array{1,2,3}}
test(S00){for{1|(2=3)|3}=array{1,3}}
test(S00){for(x:=0|1) do nat[x]}
test(S00){for(x:=0|1) do nat[x]}
test(S00){for(x:=0|1){-x}}
test(S00){for() {:nat=3}}
test(S00){for() {:nat=3} => 4}
test(S00){for(x:=0) do :nat=x}
test(S00){for(x:=0) do :nat=x => 4}
test(S00){for(x:=0|1) do :nat=x}
test(S00){for(x:=0|1) do :nat=x => 4}

# Out of order for.
test(S00){for{a},a:=9}
test(S00){for(a,1|2){-a},a:=3}
test(S00){for(x:=a|b,y:=1|2){x+y}=array{11,12,21,22},a:=10,b:=20}
test(S00){for(x:xs,y:=1|2){x+y}=array{11,12,21,22},xs:=(10,20)}
test(S00){for{a}=array{20} where a:=20}
test(S00){for(x:=10|20,y:=1|2){x+y}=array{11,12,21,22}}
test(S00){for(x:xs,y:=1|2){x+y}=array{11,12,21,22},xs:=(10,20)}
test(S00){for(x:=10|20,y:ys){x+y}=array{11,12,21,22},ys:=(1,2)}
test(S00){for(x:xs,y:ys){x+y}=array{11,12,21,22},ys:=(1,2),xs:=(10,20)}
test(S00){for(x:xs,y:ys){x+y}=array{11,12,21,22},xs:=(10,20),ys:=(1,2)}
test(S00){Length(for{x=1,x:=1|2})=1}
test(S00){Length(for{x=2,x:=1|2})=1}
test(S00){Length(for{x=3,x:=1|2})=0}
test(S00){as:=array{_} => Length[for{as[_]}]}
test(S00){rs:=for(){r:=a},a:=3}

# Testing 'for' with <abstracts>.
test(A00){for{a}=array{20} where a:any} # Perhaps this should be S00. Consider equation <abstracts> rule.
test(A00){for{_}}
test(A00){for{x:any}}
test(A00){for{x:any}=array{3}}
test(S00){for{_=(2|3)}}

# Forking for.
test(S00){for{1|2}=array{1,2}}
test(S00){for{1|2|3}=array{1,2,3}}

# Forking filtering for.
test(S00){for{1|(2=3)|3}=array{1,3}}
test(S00){for{(1|2)=3}=array{}}
test(S00){for{(1|2)=1}=array{1}}
test(S00){for{(1|2)=2}=array{2}}
test(S00){for{(1|2)=(1|2)}=array{1,2}}
test(S00){for{(1|2)=(2|1)}=array{1,2}}

# Binding for.
test(S00){for(x:=1){x}}
test(S00){for(x:=1|2){x}}
test(S00){for(x:=1|2){x}=array{1,2}}
test(S00){for(x:=1|2|3){x}=array{1,2,3}}
test(S00){for(a,1|2){-a},a:=3}

# Choice requiring new scopes.
test(S00){for{x:=1|(2,3),y:=(3,4)|2}}
test(S00){for{x:=1|(a:=1,a),y:=(b:=1,b)|2}}
test(N00){for{x:=1|(a:=1,a),a}}

# Combinatorially costly.
test(S00){for(x:=1|2|3|4,not x=2|3){x}=array{1,4}}
test(S00){for(x:=1|2|3,not x=2){x}=array{1,3}}
test(S00){for(x:=2|3|4,not x=2|3){x}}

# Length.
test(S00){function(x:=1|2){Length(for{x=1|2})=1}}

# Multiple choice 'for'.
test(S00){for(x:=10|20,y:=1|2){x+y}=array{11,12,21,22}}
test(S00){for(x:=a|b,y:=1|2){x+y}=array{11,12,21,22},a:=10,b:=20}
test(S00){operator'+'(for(x:=1|2){x})=3}
test(S00){for{operator'+'(0|2,0|1)}=array{0,1,2,3}}
test(S00){Length(for{let(){0|1|2}=1})=1}
test(S00){for{0|2,0|1}=((0,0),(0,1),(2,0),(2,1))}
test(S00){for{operator'+'(for(x:=2|1){0|x})}}

# Nested 'for'.
test(S00){(W->X:type{for(0|1){2}}) where W}
test(S00){for(r:=for(0..3) do 'X'|'Y') do Print(r)} # Made vastly slower by just element dominators logic.
test(S00){for{for(){0|1}}}
test(S00){for{xs[_],1|2} where xs:=(10,20)}
test(S00){for{for(x:=2|1){0|x}}=_}
test(R20){for{for(x:=2|1){0|x}}=((0,0),(0,1),(2,0),(2,1))} #!!CRAZY: Runtime (only) broken by 'for' length-fold.

# Container iteration.
test(S00){for(x:array{}){x+1}=array{}}
test(S00){for(x:array{1}){x+1}=array{2}}
test(S00){for(x:array{3,5}){x+1}=array{4,6}}
test(S00){for(x:array{1,3,2,4,3,5,4,6}){x}=array{1,3,2,4,3,5,4,6}}
test(S00){for{::((1,2),(3,4))}=(1,2,3,4)}
test(S00){for(xs:((1,2),(3,4)),x:xs){x}=(1,2,3,4)}

# Ranges. 
test(S00){for{1..3}=array{1,2,3}}
test(S00){for{3..7}=array{3,4,5,6,7}}
test(S00){for{1|3..7|12}=array{1,3,4,5,6,7,12}}
test(S00){for(x:=1..5) do Print(x)}
test(S00){for{0..0..3}=array{0,0,1,0,1,2,0,1,2,3}}

# Asynchronous 'for'.
test(S00){for{x=1|2,x:int}}
test(S00){for{x:int,x=1|2}}
test(S00){for{array{2,3}[i:nat]}}
test(S00){for{x:int,x=1|2}}
test(S00){a:int => for{int[a],b:=1|2,b=3}}
test(S00){a:any => for{int[a],b:=1|2,b=3}}
test(S00){for{let(x:=1|2) {x+1}}=(2,3)}
test(S00){for{x+1 where x:=1|2}=(2,3)}

# Using 'for' with choice provided by domain of user function.
test(S00){G(x:=0|1):=2; for{G[_]}}
test(S00){G(x:=0|1):=x; Print(for{G[0]})}
test(S00){G(x:int where x=(7|8)):=9; for{G[7]}}
test(S00){G(x:int where x=(0|1)):=x; Print(for{G[0]})}
test(S00){for{:function(3|5){6}}}
test(S00){for(i->x:function(j:=3|5){j+1}){i,x}}

# Array iteration expansion.
test(S00){Length[for{array{2,4,6}[i],i:=1}]}
test(S00){Length(for{array{2,4,6}[i:int]=4})=1}
test(S00){Length(for{array{2,4,6}[i:int]=5})=0}
test(S00){Length(for{array{2,4,6}[a:any]=5})=0}
test(S00){if{i where array{2,4,6}[i:int]=4}=1}

#
# Simple macros.
#

# operator'&'.
test(S00){a&b:=3}
test(S00){a&b:=3; a=3; b=3}
test(F00){a&b:=3; a=4}
test(F00){a&b:=3; b=4}

# operator'=' is the unify macro.
test(S00){a:=operator'='{}}
test(S00){a:=operator'='{2},a=2}
test(F00){a:=operator'='{2},a=3}

# 'let'.
test(S00){let(){1+2;3+4}}
test(S00){let(x:=1){y:=2,x+y}}
test(S00){let(x:=1). x+1}
test(S00){let(x:=2,y:=3){y+x}=5}
test(N00){let(x:=1){},x}
test(N00){let(){x:=1},x}
test(N00){let(x){x:=1}}

# 'where'.
test(S00){1 where 2}
test(S00){x:=1 where y:=x+1}
test(S00){x:=y+1 where y:=1}
test(S00){(1 where 2)=1}
test(F00){(1 where 2)=2}
test(S00){(x:int where x+1=3)=2}
test(F00){(x:int where x+1=3)=1}

# 'not'.
test(S00){not 1=2}
test(S00){not (1=2)}
test(F00){not 1=1}
test(F00){not 1}
test(F00){not 1|2}

# 'forall'.
test(S00){forall(1). 2}
test(F00){1=forall(1). 2}
test(S00){2=forall(1). 2}
test(S00){forall(x:=2|3). 4}
test(S00){4=forall(x:=2|3). 4}
test(F00){5=forall(x:=2|3). 4}
test(S00){forall(x:=2|2). x}
test(S00){2=forall(x:=2|2). x}
test(F00){3=forall(x:=2|2). x}
test(F00){forall(x:=2|3). x}
test(S00){1=forall(x:false). 5}
test(A00){forall(:false). 5}
test(A00){forall{:false}}
test(S00){forall{2}}
test(S00){forall{2|2}}
test(F00){forall{2|3}}

# 'reject' macro for first-class static rejection.
test(V02){reject{"Bad"}}
test(V02){reject{_}}
test(C00){1(2),reject{"Bad"}}
test(V02){reject{"Bad"},1(2)}
test(V02){r:=reject{r}} # Both reject and its parameter cast non-paraconsistently.
test(V02){f(x:false):=reject{x}}

# 'or'.
test(S00){1 or 2}
test(S00){false? or 2}
test(S00){1 or false?}
test(S00){(1 or 2)=1}
test(F00){(1 or 2)=2}
test(S00){(false? or 2)=2}
test(S00){(1 or false?)=1}
test(S00){(1 or 2 or 3)=1}
test(S00){(false? or 2 or 3)=2}
test(S00){(false? or false? or 3)=3}
test(S00){(1 or 2 or 3)=1}
test(S00){(false? or 2 or 3)=2}
test(S00){(false? or false? or 3)}
test(F00){false? or false?}
test(F00){(false? or 2)=1}
test(S00){(false? or 2)=2}
test(F00){false? or false?}

# 'first'.
test(S00){first{3}=3}
test(F00){first{3=4}}
test(F00){first{3}=4}
test(S00){first(x:=1|2){x+1}=2}
test(F00){first(x:=1|2){x+1}=3}
test(S00){first{first{false?|3}}=3}

# 'check'.
test(S00){check{123}}
test(F00){check{1=2}}
test(S00){check{123}=123}
test(F00){check{123}=456}
test(D00){a:int => check{a=3}}
test(D00){a:int => check{123}=a}
test(S00){check{123} => 456}
test(F00){check{1=2} => 456}
test(S00){check<decides >{123} => 456}
test(S01){check<fails   >{122} => 456}
test(S00){check{x:int=3}}

# 'assert'.
test(S00){assert{1=1}}
test(R01){assert{1=2}}

#
# Simple program structure.
#

# Syntactic equivalences.
test(S00){x is 3; Print(x)}
test(S00){f(x in int):=x; f(3)=3}

# Commas, semicolons, and significant indent.
test(S00){a:=(1),a=1}
test(S00){a:=(1,2),a=(1,2)}
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
test(F00):
    x:=
        1
        2
    x=1
test(S00):
    x:=
        1
        2
    x=2
test(S00){f:=function(){2},f()=2}
test(F00){f:=function(){2},f()=3}
test(S00){f:=function(){2;},f()=2}
test(S00){f:=function(){2;3;},f()=3}
test(F00){f:=function(){2;3;},f()=2}

# Combinatorial performance check.
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

#
# General unification.
#

# Miscellaneous.
test(A00){x:='a',-x} # Could be U00 but missing axiom.
test(A00){-'a',x:=3} # Same.

# Abstract head shape rejection.
test(F00){a:int => b:float => a=b}
test(F00){a:int => a=(3,4)}
test(F00){a:float32 => b:float64 => a=b}
test(F00){a:int => a=int}
test(F00){a:int => a='3'}
test(F00){a:int => '3'=a}
test(D00){a:int => a=3}
test(D00){a:int => 3=a}

# Simple comparability.
test(S00){a:int   => f(x:nat):int => if{f[a]=1}}
test(D00){a&b:int => f(x:int):=x => {f[a]=f[b]}}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(D00){f(x:int):int => a:int => f[a]=a}
test(D00){f(x:int):int => s:=7 => f[s]=s}
test(D00){f(x:int):int => f[7]=7}

#
# Verse types in depth.
#

# Number typess and values.
test(S00){int[-123]}
test(S00){nat[123]}
test(S00){int[123]}
test(F00){nat[-123]}
test(S00){nat[4/2]}
test(F00){nat[2/3]}
test(F00){int[2/3]}
test(S00){rational[123]}
test(S00){rational[-123]}
test(S00){rational[2/3]}
test(S00){rational[-2/3]}

# Number type inclusion.
test(S00){f(x:any     ):=any[x]}
test(S00){f(x:nat     ):=any[x]}
test(S00){f(x:int     ):=any[x]}
test(S00){f(x:rational):=any[x]}
#
test(U00){f(x:any     ):=rational[x]}
test(S00){f(x:nat     ):=rational[x]}
test(S00){f(x:int     ):=rational[x]}
test(S00){f(x:rational):=rational[x]}
#
test(U00){f(x:any     ):=int[x]}
test(S00){f(x:nat     ):=int[x]}
test(S00){f(x:int     ):=int[x]}
test(D00){f(x:rational):=int[x]}
#
test(U00){f(x:any     ):=nat[x]}
test(S00){f(x:nat     ):=nat[x]}
test(D00){f(x:int     ):=nat[x]}
test(D00){f(x:rational):=nat[x]}

# Number type inclusion, tested again using functions.
test(S00){f():any      => any[f[]]}
test(S00){f():nat      => any[f[]]}
test(S00){f():int      => any[f[]]}
test(S00){f():rational => any[f[]]}
#
test(U00){f():any      => rational[f[]]}
test(S00){f():nat      => rational[f[]]}
test(S00){f():int      => rational[f[]]}
test(S00){f():rational => rational[f[]]}
#
test(U00){f():any      => int[f[]]}
test(S00){f():nat      => int[f[]]}
test(S00){f():int      => int[f[]]}
test(D00){f():rational => int[f[]]}
#
test(U00){f():any      => nat[f[]]}
test(S00){f():nat      => nat[f[]]}
test(D00){f():int      => nat[f[]]}
test(D00){f():rational => nat[f[]]}

# Characters.
test(F00){char[1]}
test(S00){char['X']}
test(S00){char8['X']}
test(D00){f(x:int):=char[x]} # Will be tightened to F00.
test(D00){f(x:char):=int[x]} # Will be tightened to F00.

# Casting with multiple heads doesn't lead to fixed point loops because atoms are unique.
test(S00){x:=0,x=-x}
test(S00){as:=array{a:=0}; as[a]=a}

# Void.
test(S00){x:any => void[x]=false}

# operator'?'.
test(A00){int?}
test(S00){int?=3}
test(F00){int?='3'}
test(S00){type{3}?}

# 'known'.
test(S00){known[123]}
test(S00){known[3,5]}
test(U00){x:int=>known[x]}
test(U00){x:int=>known[3,x]}
test(S00){x:=4=>known[3,x]}
test(A00){known[x:=x]}

#
# Verse natives in depth.
#

# 'Concatenate'.
test(S00){array{}=Concatenate[]}
test(S00){Concatenate[array{},array{}]}
test(S00){Concatenate[array{},array{7}]}
test(S00){Concatenate[array{2},array{3}]=array{2,3}}
test(F00){Concatenate[array{2},array{3}]=array{8,9}}
test(S00){Concatenate[array{},array{3}]=array{3}}
test(F00){Concatenate[array{},array{3}]=array{8}}
test(F00){Concatenate[array{},array{7}]=array{8,9}}
test(F00){as:=array{},bs:=array{3},Concatenate[as,bs]=array{8}}
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

# Abstract natives. Not yet supported!
#test(S00){f(x:int):=x+1,f(3)=4}
#test(F00){f(x:int):=x+1,f(3)=5}
#test(S00){f(x:int):=-x,f(3)=-3}
#test(S00){f(x:int):=1+2,f(3)}
#test(S00){f(x:int):=x+10,7,f(3),8}
#test(S00){f(x:prefix'-'):=x+10,Print(f(3)),f(3)=7}
#test(S00){f(x:int):={a:=x,b:=a*x},f(1)}
#test(S00){f(x:int):=let(y:=x*2) do x+y,f(4)=12}
#test(S00){f(g(:int):int):=g(3)=>f(operator'-')}
#test(U00){f(g(:int):int):=g(3)=>f(int=operator'-')}

#
# Data flow and <decides> for comparable values.
#

# These were <resolves>, but new comparable data flow analysis made them <decides>.
test(D00){F(a:any)<resolves>:={b:=a=3; expect<succeeds>{int[b]}}} # This is the core underlying problem.
test(D00){F(a:int)(b:any)<resolves>:={c:=a=b; expect<succeeds>{int[c]}}}
test(D00){a:int => b:any => f(:int):type{a=b} => f[a=b]}
test(D00){a:int => b:any => f(a):type{a=b} => f[a=b]}

# Exploring comparable & decides data flow.
test(D00){F(a&b:any)(i:int)<resolves>:={x:=a=i; expect<succeeds>{i=3}}}
test(U00){F(a&b:any)(i:int)<resolves>:={x:=a=i; expect<succeeds>{a=3}}}
test(U00){F(a&b:any)(i:int)<resolves>:={x:=a=i; expect<succeeds>{b=3}}}
test(D00){F(a&b:any)(i:int)<resolves>:={x:=a=i; expect<succeeds>{x=3}}}
test(D00){F(a&b:any)(i:int)<resolves>:={x:=a=i; check<succeeds>{i=3}}}
test(U00){F(a&b:any)(i:int)<resolves>:={x:=a=i; check<succeeds>{a=3}}}
test(U00){F(a&b:any)(i:int)<resolves>:={x:=a=i; check<succeeds>{b=3}}}
test(D00){F(a&b:any)(i:int)<resolves>:={x:=a=i; check<succeeds>{x=3}}}
test(S00){f(a:any,b:int)<decides>:=x:=a=b where if(x=3) then false? else false?}

# Checking that comparable-flow works as desired.
test(U00){f(a:any,c:int)<decides>:=a=c}
test(S00){f(a:any,c:int)<resolves>:={x:=a=c; if{x=3}}}
test(S00){f(a:any,c:int)<resolves>:={x:=a=c; expect<decides>{x=3}}}
test(S00){f(a:any,c:int)<resolves>:={xs:=array{c}; if{xs[0]=3}; xs=array{a}}}
test(U00){f(a:any,c:int)<decides>:={x:=a=c; expect<decides>{x=3}}}

# Exploring whether comparable rep-flow is sensible.
test(D00){x:any => assume<succeeds>{comparable[x]} where x=3}
test(D00){x:any => assume<decides >{comparable[x]} where x=3}
test(D00){x:any => assume<resolves>{comparable[x]} where x=3}

#
# First-class 'comparable' type.
#

# Basic tests.
test(S00){comparable[3]}
test(U00){a:any => comparable[a]}
test(S00){a:int => comparable[a]}
test(S00){a:comparable => comparable[a]}
test(U00){a:any => comparable[a,3]}
test(S00){F(a:any)(f(:int):comparable)<resolves>:={x:=f[a]}}
test(D00){F(a:int)(f(:nat):comparable)<resolves>:={x:=f[a]; expect<succeeds>{x=3}}}
test(D00){F(a:any)(f(:nat):comparable)<resolves>:={x:=f[a]; expect<succeeds>{x=3}}}

# We haven't implemented recursive comparability yet.
#test(S00){a:int => comparable[a,3]}
#test(S00){a:comparable => comparable[a,3]}
#test(S00){comparable[3,5]}
#test(S00){a:comparable => comparable[a,5]}

#
# Macro problems.
#

# Bad macro parameters.
test(M02){array(){}}
test(M02){for<a>(b)<c>{d}}
test(M02){function<succeeds>(){3}}

# Macro resolution inconsistent across control flow exploration.
test(S00){f(m:=let):=m(x:=2){x+1=3}}
test(S00){f(m:=for):=m(x:=2){x+1}=array{3}}
test(V04){type{(for|first){}}}
test(V04){f(m:=let|for):=m(1){2}}
test(V04){for(m:=let|for) do m(1){2}}

# Code generation divergence.
test(S00){for(i:=0..1){m:any; for{}; if(i=0) then m=for; if(i=1) then m=for}}
test(M01){for(i:=0..1){m:any; if(i=0) then m=for; m{}; if(i=1) then m=for}}
test(M01){for(i:=0..1){m:any; if(i=1) then m=for; m{}; if(i=0) then m=for}}
test(S00){for(i:=0..1){m:any; m=for; m{}}}
test(S00){for(i:=0..1){m:any; m{}; m=for}}

#
# Relevance.
#

test(A00){1 where a:any} # Could tighten to S00.
test(A00){1 where a:=a}  # Could tighten to S00.

#
# Cycles in containers and functions.
#

# Arrays and circularity.
test(A00){array{x:=x}}
test(S00){array{x:=x}; x=3}
test(A00){array{x:=x,y:=y}}
test(A00){array{x:=x,y:=y}; x=3}
test(A00){array{x:=x,y:=y}; y=4}
test(S00){array{x:=x,y:=y}; x=3; y=4}
test(A00){array{x:=y,y:=x}}
test(S00){array{x:=y,y:=x}; x=3}
test(S00){array{x:=y,y:=x}; y=3}
test(A00){(x:=x,y:=y)=(a:=a,b:=b)}

# Function calling can loop circularly.
test(S00){f():=2 => f=2 => f[]=3}
test(D00){a:int => (f(7):=8) where f[7]=a}
#test(F00){f(x:int):=x=function(1){2}}
#test(F00){f(x:int):=x=2=function(1){2}}
#test(F00){f(x:int):=x=function(1){2}=function(3){4}}

# Convergence of circular function definitions.
test(S00){f(1):=f}
test(S00){f(f):=1}
test(S00){f(f):=f}
test(S00){f(x:=f):=x}
test(S00){f(1):=f; f}
test(S00){f(1):=f; f[1]}
test(S00){f(1):=f; f(1)}
test(S00){f(f):=1; f[f]}
test(S00){f(f):=1; f(f)}
test(S00){f(f):=f; f[f]}
test(S00){f(f):=f; f(f)}
test(S00){f(x:=f):=x; f[f]}
test(S00){f(x:=f):=x; f(f)}
test(S00){f():=f; f[]=f}
test(R04){f():=f,g():=g,f=g}
test(U00){f():=f,g():=g,f[]=g[]}
#test(U00){f():=1,g():=2,(f=g)()}

#
# Nesting functions, type, iteration, and abstraction.
#

# Abstraction propagation.
test(S00){f():=for(1) do :int => 0}
test(S00){f():=for(1|2) do :int => 0}
test(S00){f():=if(1=1) then :int else :nat => 0}
test(S00){f():=if(1=2) then :int else :nat => 0}
test(S00){f():=let() do :int => 0}
test(S00){f():type{(:nat)|(:int)} => 0}

# Iteration inside function domains and types.
test(S00){function(for(x:=3|4){5}){6}[5,5]=6}
test(S00){function(for(x:=3|4){x+1}){7}[4,5]=7}
test(S00){function(for{3|4}){8}[3,4]}
test(S00){function(as:=for(x:=1|2){3}){as}[3,3]=(3,3)}
test(S00){function(as:=for(x:=1|2){:int}){as}[3,4]=(3,4)}
test(S00){function(as:=for(x:=1|2){:prefix'-'}){as}[3,4]=(-3,-4)}
test(S00){function(1 where as:=for{x:=1|2|3}){as}[1]=(1,2,3)}
test(F00){function(for{3|4}){8}[4,5]}
test(S00){a:type{x:int,if{x=3}} => 1}
test(S00){f():type{x:int,if{x=3}} => 1}
test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,4]}
test(S00){for{array{2,4,6}[_]=4}=array{_}}
test(U00){x:int => if{x=3,f():=x=3}}
test(S00){g(9):=8,f(9 where for{x:=1}):=8,f[9],-1}
test(U00){a:any=>{f(if{c=1}):=8,c:=a}}
test(S00){f(if{c=1}):=8,c:=7}
test(S00){if{f(if{c=1}):=8},c:=7,c=7}
test(S00){if{f(if{c=1}):=8},c:=7}
#test(S00){t:=type{a:int,b:if(a=1) then int else char}; t[1,3]; t[2,'x']; not t[1,'3']; not t[2,3]}

#
# Specifiers.
#

# Checking effects specifiers.
test(X30){f()<1>:=2}
test(X30){function()<2>{}}
test(X30){f(x:false):=function()<x>{}}
test(X30){f(x:=2=3):=function()<x>{}}

# Function specifier allowance.
test(S00){f(0)<closed>:=1}
test(S00){f(0)<open>:=1}
test(S00){f(0)<converges>:=1}
test(S00){f(0)<computes>:=1}
test(X30){f(0)<covariant>:=1}
test(S00){assume<succeeds>{1}}
test(X31){assume<closed>{1}}
test(S00){expect<succeeds>{1}}
test(X31){expect<open>{1}}

# Weird comparisons.
test(S00){decides=decides}
test(F00){decides=fails}
test(S00){array=array}
test(U00){array=for}

# Testing ':' specifiers.
test(S00){x:int,x=3}
test(S00){:void=3}
test(S00){:void=false}
test(V10){a:int => :int=a=3} # Would prefer D00 to prioritize above V10.
test(A00){:int<succeeds>}
test(S00){:int<succeeds>=3}
test(S00){:int<decides>=3} # Because declared fx act as a mask on top of existing LambdaFx.
test(S00){:int<computes>=3}
test(X00){:void<computes>=Print("Hello")}
#need better errir nessage test(X31){:int<open>=3}

#
# Error priorities.
#

test(F00){x:int => f():=x=1|2 => {f[],first{x=1}}}
test(F00){x:int => f():=x=1|2 => {first{x=1},f[]}}
test(N04){f(g():any):=g[]; f(g():=6)}
test(N04){f(x:int)<decides>:=x=3, f[x:int]}

#
# Advanced: Testing Primitives.
# These exist for testing the verifier.
#

# Testing tests.
test(S00){test(S00){123}}
test(T01){test(F00){123}}
test(F00){test(S00){2=3}}
test(S00){test(S00){Print(3)}=>3}

# Note that we have computed and nested tests.
# These are disabled because, for unknown reasons, top-level 'for' (even with known finite iteration) affects performance combinatorially.
#for(i:=0|1) do test(F00){i=3}
#for(i:=0|1) do test(S00){nat[i]}

# Native 'expect' safely checks expected effects, including a subset of what's allowed in the context.
test(S00){expect<succeeds>{1}}
test(F00){expect<succeeds>{1=2}}
test(S00){expect<succeeds>{1}=1}
test(F00){expect<succeeds>{1}=2}
test(F00){if{expect<succeeds>{1=2}}}
test(F00){if{expect<succeeds>{1=2}}}
test(S00){if{expect<decides>{1=2}}}
test(S00){if{expect<fails>{1=2}}}
test(F00){if{expect<succeeds>{false[]}}}
test(S00){f(x:int):=expect<succeeds>{x},f[3]=3}
test(F00){f(x:int):=expect<succeeds>{x},f[3]=4}
test(D00){x&y:int => if{expect<succeeds>{x=y}}}
test(S00){x&y:int => if{z:=x=y,expect<succeeds>{z}}}
test(D00){x&y:int => if{z:=x=y,expect<succeeds>{z=x}}}
test(D00){x&y:int => if{z:=x=y,expect<succeeds>{z=y}}}
test(D00){a&b:int => if{x:=a=b  ,expect<succeeds>{x=a}}}
test(D00){a&b:int => if{x:=a,x=b,expect<succeeds>{x=a}}}
test(S00){a&b:int => if{x:=a,a=b,expect<succeeds>{x=a}}}
test(S00){for{x:=1,expect<succeeds>{x=1}}}
test(F00){for{x:=1|2,expect<succeeds>{x=1}}}
test(F00){for{x:=1|2,expect<succeeds>{x=2}}}
test(S00){for{x:=1|1,expect<succeeds>{x=1}}}
test(S00){as:=for{array{2,4,6}[i:int]=4}; Length(as)=1}
test(F00){for{array{2,4,6}[i:int]=4,expect<succeeds>{i=2}}}
test(S00){j:=for{i where array{2,4,6}[i:int]=4},expect<succeeds>{j=array{1}}}
test(F00){as:=for{i where array{2,4,6}[i:int]=4}; as=array{2}}
test(S00){x:int   => expect<succeeds   >{x}}
test(S00){x:int   => expect<succeeds   >{x}}
test(S01){x:int   => expect<contradicts>{x}}
test(S01){x:int   => expect<fails      >{x}}
test(S00){x:false => expect<succeeds   >{x}}
test(S00){x:false => expect<fails      >{x}}
test(S00){if{expect<succeeds>{1}}}
test(F00){if{expect<succeeds>{1=2}}}
test(F00){if{expect<succeeds>{false[]}}}
test(D00){a:int => if{expect<succeeds>{a=1}}}
test(S00){a:int => if{expect<decides>{a=1}}}

# Native 'assume' unsafely injects unproven assumptions with a given strength.
test(D00){x:int => x=1}
test(S00){x:int => assume{x=1} where x=1}
test(S00){x:int => x=1 where assume{x=1}}
test(F00){x:int => assume{x=1} where x=2}
test(F00){x:int => x=2 where assume{x=1}}
test(S00){x:=assume{y:=assume{z:=x}}}
test(S00){assume<succeeds>{123}}
test(N00){assume{bogus}}
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
#Loops following RunComposeStep change: test(S00){a&b:int => {assume{x=a},x:=a=b}}
#Loops following RunComposeStep change: test(S00){a&b:int => {assume{x=b},x:=a=b}}
#Loops following RunComposeStep change: test(R20){assume{x=3},x:=4}
test(S00){r:=assume{x},x:=r,x=3}
test(S00){a&b:int => {x:=a=b,assume{x=a},assume{x=b}}}
test(S00){x:=x=assume{x}}
test(D00){a&b:int => f():=a => f()=b}
test(S00){a:int => {f():=assume{a=3}}}
test(D00){a:int => {f():=assume{a=3}; a=3}}
test(S00){a:int => {f():=assume{a=3}; f[]; a=3}}
test(D00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=a}}}
test(D00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{a=c}}}
test(S00){assume{f():int}}
test(R00){g:=assume{f():int}; g[]}
test(D00){g:=assume{f():int}; g[]=2}
test(R00){g():=assume{:int}; g[]}
test(D00){g():=assume{:int}; g[]=2}
test(S00){x:int => assume<succeeds>{x=2}}
test(D00){x:int => assume<decides>{x=2}}
test(S00){x:int => assume<succeeds>{assume<decides>{x=2}}}
test(S00){x:int => assume<decides>{assume<succeeds>{x=2}}}
test(S00){assume{1}=assume{1}}
test(S00){1=assume<decides>{1}}
test(S00){1=assume<abstracts>{1}}
test(F00){assume{1}=assume{2}}
test(F00){assume{1}=2}
test(S00){a&b:int => assume{a}=assume{a}}
test(S00){a&b:int => assume{a}=a}
test(D00){a&b:int => assume{a}=assume{b}}
test(D00){a&b:int => assume{a}=b}
test(U00){a&b:any => assume{a}=assume{b}}
test(U00){a&b:any => assume{a}=b}

#
# Advanced: Function-equality inference.
#

# Assumption monotonicity.
test(S00){a:int => if{x:=a=2,expect<succeeds>{x}}}
test(S00){a:int => if{x:=a=2,assume{a=1}}}

# A call that doesn't succeed should block function equality. f[a]=b <=> f=function(a){b}. 
test(U00){F(a:any  )(f(x:nat):any)<resolves>:={g:=expect<succeeds>{f}; g[a]}}
test(D00){F(a:int  )(f(x:nat):any)<resolves>:={g:=expect<succeeds>{f}; g[a]}}
test(S00){F(a:nat  )(f(x:nat):any)<resolves>:={g:=expect<succeeds>{f}; g[a]}}
test(U00){F(a:any  )(f(x:nat):int)<resolves>:={g:=expect<succeeds>{f}; g[a]=3}}
test(D00){F(a:int  )(f(x:nat):int)<resolves>:={g:=expect<succeeds>{f}; g[a]=3}}
test(D00){F(a:nat  )(f(x:nat):int)<resolves>:={g:=expect<succeeds>{f}; g[a]=3}}
test(U00){F(a:any  )<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]}}
test(D00){F(a:int  )<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]}}
test(S00){F(a:nat  )<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]}}
test(U00){F(a&b:any)<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]=b}}
test(D00){F(a&b:int)<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]=b}}
test(D00){F(a&b:int)<resolves>:={f:=expect<succeeds>{function(x:nat){3}}; g:=expect<succeeds>{f}; g[a]=b}}

# Same, now looking beyond domain check.
test(S00){F(a:int)(f(x:int):int)<decides>:={g:=expect<succeeds>{f}; y:=g[a]}}
test(D00){F(a:int)(f(x:nat):int)<decides>:={g:=expect<succeeds>{f}; y:=g[a]}}
test(D00){F(a:int)(f(x:int):int)<decides>:={g:=expect<succeeds>{f}; y:=g[a]=3}}

#
# Advanced: Call-equality inference considering effects.
#

# Pure function call equality.
test(D00){f(x:int):int           => f[3]=f[3]}
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(S00){f(x:any)<computes>:any => f[3]=f[3]}
test(D00){f(x:int)<computes>:int => f[3]=f[4]}
test(U00){f(x:int)<computes>:any => f[3]=f[4]}
test(S00){(f(:int)<computes>:int,f[3]=4) => f[3]=4}
test(S00){(f(:int)<computes>:int,f[3]=4) => f(3)=4}
test(F00){(f(:int)<computes>:int,f[3]=4) => f[3]=5}
test(F00){(f(:int)<computes>:int,f[3]=4) => f(3)=5}
test(U00){a&b:any => f(_    )<computes>:any => f[a]=f[b]}
test(U00){a&b:any => f(x:any)<computes>:=x  => f[a]=f[b]}
test(D00){a&b:int => f(x:int)<computes>:=x  => f[a]=f[b]}
test(S00){f(:any)<computes>:any              => f[2]=3  => f[2]=3}
test(S00){f(:any)<computes>:any              => f(2)=3  => f[2]=3}
test(F00){f(:any)<computes>:any              => f(2)=3  => f[2]=4}
test(U00){f(:any)<computes>:any              => f(2)=3  => f[3]=3}
test(U00){f(:any):any                        => f[2]=3  => f[2]=3} # Not <computes>
test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=as}
test(F00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=(_,_,_)}

# Checking soundness in detail.
test(D00){F(a&b:int)(f(:int)<computes>:int)<resolves>:=a=b where expect<succeeds>{f[a]=f[b]}}
test(U00){F(a&b:int)(f(:int)<computes>:any)<resolves>:=          expect<succeeds>{f[a]=f[b]}} 
test(S00){F(a&b:int)(f(:int)<computes>:any)<resolves>:=          expect<succeeds>{if{a=b}}}
test(D00){F(a&b:int)(f(:int)<computes>:int)<resolves>:=a=b where expect<succeeds>{f(a)=f(b)}}
test(D00){F(a:int  )(f(:int)<computes>:int)<resolves>:=a=3 where expect<succeeds>{f[a]=f[3]}}
test(D00){F(a:int  )(f(:int)<computes>:int)<resolves>:=a=3 where expect<succeeds>{if{f[a]}=f[3]}}
test(U00){F(a&b:any)(f(:any)<computes>:any)(c:=f[a])(d:=f[b]):={a=b,c=d}}
test(D00){F(a&b:int)(f(:int)<computes>:any)(c:=f[a])(d:=f[b]):={a=b,c=d}}
test(U00){F(a&b:int)(f(:int)<computes>:any)(x:=f(a))(y:=f(b)):=if{f(a)=f(b)}}
test(S00){F(a&b:int)(f(:int)<computes>:any)(x:=f(a))(y:=f(b)):=if{a=b,f(a)=f(b)}}
test(D00){F(a&b:int)(f(:int)<computes>:any)(x:=f(a))(y:=f(b))<resolves>:=a=b where expect<succeeds>{x=y}}
test(S00){F(a&b:any)(f(:any)<computes>:any)(c:=f[a])(d:=f[b])(a=b):=c=d}
test(S00){F(a:any  )((f&g)(:any)<computes>:any)(c:=f[a])(d:=g[a])(f=g):=c=d}
test(S00){F(a&b:any)((f&g)(:any)<computes>:any)(c:=f[a])(d:=g[b])(a=b)(f=g):=c=d}

# Tests that were dependent on applying constraints non-locally to function.
test(F00){f(7)<computes>:type{y:=y} => f[7]=8 => f[7]=9}
test(F00){a:any => f(:any)<computes>:any => f[a]=1 => f[a]=2}
test(F00){f(:any)<computes>:any => f[2]=3 => f[2]=4}
test(D00){a:int => f(7)<computes>:type{y:=y} => f[7]=a => f[7]=9}
test(S00){f(x:int)<computes>:any => a&b:int => a=b => f[a]=f[a]}

# Asynchronous matching.
test(S00){f(:int)<computes>:int => a:int => {y:=a,f(a)=f(y)}}
test(S00){f(:int)<computes>:int => a:int => {f(a)=f(y),y:=a}}
test(S00){f(:int)<computes>:int => a:int => {f(3)=f(y),y:=3}}
test(D00){f(:int)<computes>:int => a&b:int => {f[a]=f[y],y:=b}}
test(U00){f(:int)<computes>:any => a&b:int => {f[a]=f[y],y:=b}}
test(S00){f(:int)<computes>:int => a&b&c:int => f(a)=b => f(a)=c => b=c}

# Call matching.
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

# Function result unification.
test(S00){(f&g)(x:int)<computes>:int => f[3]=f[3]}
test(D00){(f&g)(x:int)<computes>:int => f[3]=f[4]}
test(D00){(f&g)(x:int)<computes>:int => f[3]=g[3]}
test(U00){(f&g)(x:int)<computes>:int => if{f=g}}
test(U00){f(x:int)<computes>:any => a&b&c:int => if{f[a]=f[c]}}
test(S00){f(x:int)<computes>:any => a&b&c:int => a=b => if{f[a]=f[b]}}
test(S00){f(x:int)<computes>:any => a:int => if{a=3,f[a]=f[a]}}

# Atom coherency.
test(S00){f()<computes>:int => f()=f()}
test(S00){f(x:int)<computes>:int => f(1)=f(1)}
test(S00){f(x:int)<computes>:int => f(274876858367*4398042316799)=f(1208920055006831267807233)}

# Pure function equality.
test(S00){a&b:any   => f(:any)<computes>:any => f[a]=b => f[a]=b}
test(U00){a&b&c:any => f(:any)<computes>:any => f[a]=b => f[a]=c}
test(S00){a:any     => f(:any)<computes>:any => f[a]=1 => f[a]=1}
test(F00){a:any     => f(:any)<computes>:any => f[a]=1 => f[a]=2}
test(S00){f(:any)<computes>:any => f(2)=3 => f[2]=3}
test(F00){f(:any)<computes>:any => f(2)=3 => f[2]=4}

# Random stuff broken by eta.
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(D00){f(x:int):int => f[3]=f[3]}
test(S00){f(:int,:int)<computes>:int => f[3,4] where f[3,4]}
test(S00){f(:int,:int)<computes>:int => f[3,4]=f[3,4]}
test(D00){f(:int,:int)<computes>:int => f[3,4]=f[4,5]}

# Calling assumed function in its own context.
test(S00){(f(x:int)<computes>:int,a:=f(3)) => f(3)=a}
test(S00){f(:int)<computes>:int=>{f(3),f(3)}}
test(D00){f(:int):int=>{f(3)=f(3)}}

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

# Probing context_domain strength dependence on matching function&parameter strength.
test(S00){F(a&b:int)(f(:nat)<computes>:any)(f[a])<resolves>:=a=b where expect<succeeds>{f[a]}}
test(S00){F(a&b:int)(f(:nat):any          )(f[a])<resolves>:=a=b where expect<succeeds>{f[a]}}
test(D00){F(a&b:int)(f(:nat)<computes>:any)(f[a])<resolves>:=a=b where expect<succeeds>{f[b]}}
test(D00){F(a&b:int)(f(:nat):any          )(f[a])<resolves>:=a=b where expect<succeeds>{f[b]}}
#test(D00){F(a:int)((f&g)(:nat):int)(f[a])<resolves>:=f=g where expect<succeeds>{g[a]}} # Pending call-multiple-functions.

# Pure and impure functions.
test(S00){f(:int)<computes>:int   => f[3]=f[3]}
test(D00){f(:int):int             => f[3]=f[3]}
test(D00){f(:int):int=3;  f[3]=f[3]}
test(S00){f(:int):int<computes>=3; f[3]=f[3]}

# Past ForVisible breakage.
test(D00){f():int => g():char => f[]=g[]}
test(D00){f()<computes>:int => g()<computes>:int => f[]=g[]}
test(D00){f()<computes>:int => g():int => f[]=g[]}
test(D00){f():int => g()<computes>:int => f[]=g[]}
test(D00){f():int => g():int => f[]=g[]}
test(D00){f(x:int):int => f[3]=f[3]}
test(D00){f(x:int)<computes>:int => f[3]=f[4]}

# Unification strength.
test(S00){f(x:int):=x => a:int => f[a]=f[a]}
test(D00){f(x:int):=x => a&b:int => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(D00){f(x:int):=x => a&b:int => {f[a]=f[b],a=b}}
test(S00){f(x:int):=x => a:int => f[a]=f[a]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(D00){f(x:int):=x => a&b:int => {f[a]=f[b],a=b}}

#
# Foolproof equality constraint tests.
#

# Simple.
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[a]               => h[x]=h[y]}
test(D00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[b]               => h[x]=h[y]}
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=f[b] => a=b        => h[x]=h[y]}
test(D00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[a]               => h[x]=h[y]}
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[a] => f=g        => h[x]=h[y]}
test(D00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => a=b        => h[x]=h[y]}
test(D00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => f=g        => h[x]=h[y]}
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => a=b => f=g => h[x]=h[y]}
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => x:=f[a] => y:=g[b] => f=g => a=b => h[x]=h[y]}
test(S00){a&b:int=>(f&g&h)(:int)<computes>:int => f[a]=g[b] => h[f[a]]=h[g[b]]}

# Arrays.
test(S00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[a,c]               => h[x]=h[y]}
test(D00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[b,c]               => h[x]=h[y]}
test(S00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=f[b,c] => a=b        => h[x]=h[y]}
test(D00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[a,c]               => h[x]=h[y]}
test(S00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[a,c] => f=g        => h[x]=h[y]}
test(D00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => a=b        => h[x]=h[y]}
test(D00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => f=g        => h[x]=h[y]}
test(S00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => a=b => f=g => h[x]=h[y]}
test(S00){a&b&c:int=>(f&g)(:int,:int)<computes>:int => h(:int)<computes>:int => x:=f[a,c] => y:=g[b,c] => f=g => a=b => h[x]=h[y]}

# Function equates requiring (ComputesCallEqual).
test(S00){f()<computes>:any => f[]=3 => f[]=3}
test(U00){a&b:any => (f&g)(:any)<computes>:any => f[a]=g[b] => f[a]=g[a]}
test(D00){a:any => f(:any)<computes>:any => int[f[a]] => nat[f[a]]}
#!!test(S00){a&b:any => (f&g)(:any)<computes>:any => f[a]=g[b] => f[a]=g[b]}
#!!test(S00){a:any => f(:any)<computes>:any => int[f[a]] => int[f[a]]}
#!!test(S00){a:any => f(:any)<computes>:any => nat[f[a]] => int[f[a]]}
#!!test(S00){f(:any)<computes>:any => as:=(_,_) => f[2]=as => f[2]=(_,_)}
#!!test(S00){f(:any)<computes>:any => f[2]=(_,_) => f[2]=(_,_)}
#test(S00){f(:any)<computes>:any => f(2)=(_,_) => f[2]=(_,_)}

# Fixed unsoundness involving (ComputesCallEqual) on a suceeding and contended call.
test(S00){f():=4; f[]}
test(F00){f():=4; f[]=3}
test(F00){f():=4; f[]; f[]=3}
test(F00){t:=any; t; t[8]=9}
test(F00){t():=int; t[][3]=t[][4]}
test(F00){t():=any; t[][8]=9}
test(F00){t():=any; t[]; t[][8]=9}
test(F00){t():=any; (a:=t)[A:=()]; (c:=(b:=t))[B:=()][C:=8]=9}
test(F00){t():=any => {t[]; t[][8]=9}}
test(F00){t():=any => t[] => t[][8]=9}
test(F00){f():=int => f()[3]=f()[4]}
test(S00){f():int => f[]}
test(D00){f():int => f[]=f[]}
test(S00){f()<computes>:int => f[]=f[]}

# New eta breakage.
test(D00){f(x:int)<computes>:int => f[3]=4}
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(U00){f(x:int)<computes>:type{y:=y} => f[3]=f[4]}
test(D00){f(x:int)<computes>:int => f[3]=f[4]}
test(F00){f()<computes>:=3 => g()<computes>:=4 => c:=f()=g()}
test(S00){a:any=>int[a]=>comparable[a]}

#
# Advanced: Effective assumptions.
#

# Simple.
test(S00){x:=x => x=1 => x=1}
test(S00){x&y:any => x=y => x=y}
test(D00){x:int => x=1}
test(S00){x:int => x=1 => x=1}
test(F00){x:int => x=2 => x=1}

# Through function application.
test(D00){a:int => f():=a=1 => 2}
test(S00){a:int => f():type{a=1} => 2}
test(D00){a:int => f():type{a=1} => a=1}
test(S00){a:int => f():type{a=1} => {f[]; a=1}}
test(S00){a:int => f():type{a=1} => {a=1; f[]}}
test(F00){a:int => f():type{a=1} => {f[]; a=2}}
test(F00){a:int => f():type{a=1} => {a=2; f[]}}

#
# Advanced: Functions modulo effective assumptions.
#

# Functions and parameters not seen by direct visibility.
test(D00){x:int=>{x=3,x+1=4}}
test(F00){x:int=>{x=3,x+1=5}}
test(C00){f:any => f[_]=3}
test(S00){f:any => f=int => f[_]=3}
test(F00){f:any => f=int => f[_]='x'}
test(S00){f:any => f[_]=3 where assume{f=int}}
test(S00){p:any => p=2 => p+3=5}
test(F00){p:any => p=2 => p+3=6}
test(A00){x:any; x+3=5}
test(A00){x:any; operator'+'[x,3]=5}
test(S00){x:any; assume{x=2}; x+3=5}
test(F00){x:any; assume{x=2}; x+3=6}
test(A00){x:any; int[x]}
test(S00){x:any; assume{x=2}; int[x]}
test(S00){x:any; assume{x=2}; x=2}
test(R20){x:any; assume{x=2}; x=3}
test(S00){x:any => x=2 => x=2}
test(F00){x:any => x=2 => x=3}
test(S00){x:any => x=2 => x+3=5}
test(F00){x:any => x=3 => x+3=5}
test(S00){x:any => x+3=5 where assume{x=2}}
test(F00){x:any => x+3=5 where assume{x=3}}
test(S00){f:any => f[_]='x' where assume{f=char}}
test(F00){f:any => f[_]='x' where assume{f=int}}

# Unseen functions.
test(S00){f(x:int):=x, a:any => a=f => a[_]=3}
test(F00){f(x:int):=x, a:any => a=f => a[_]='3'}

#
# Advanced: Scalar data flow modulo effective assumptions.
#

# Basic tests to validate assumption-free scalar flow.
test(U00){x:=x => y:=y => x=y}
test(U00){x:any => y:any => x=y}
test(U00){x:int => y:any => x=y}
test(D00){x:int => y:int => x=y}
test(D00){as:=array{:int,:any} => bs:=array{:int,:any} => as[0]=bs[0]}
test(U00){as:=array{:int,:any} => bs:=array{:int,:any} => as[1]=bs[1]}
test(U00){as:=array{:int,:any} => bs:=array{:int,:any} => as[1]=bs[0]}
test(U00){as:=array{:int,:any} => bs:=array{:int,:any} => as[0]=bs[1]}
test(D00){as:=array{:int,:any} => bs:=array{:any,:int} => as[0]=bs[1]}
test(U00){as:=array{:int,:any} => bs:=array{:any,:int} => as[0]=bs[0]}
test(U00){as:=array{:int,:any} => bs:=array{:any,:int} => as[1]=bs[1]}
test(U00){as:=array{:int,:any} => bs:=array{:any,:int} => as[1]=bs[0]}

# Basics.
test(D00){x:=assume{:int}; x=3}
test(U00){a:=a => {x:=a=3; assume{a}}}

# Core distinction of <succeeds> and <decides> data flow:
# Though succeeds{u=v} moots effect of u=v, decides{u=v} doesn't moot effect of u=v.
test(S00){a&b:any => x:=a=b where assume<succeeds>{x=b}}
test(U00){a&b:any => x:=a=b where assume<decides >{x=b}}
test(D00){a&b:any => x:=a   where assume<decides >{x=b}}

# Synthetic succeeds of effective <succeeds> assumptions.
test(S00){a&b:any => assume{1}}
test(S00){a&b:any => assume{1=2}}
test(S00){a&b:any => assume{a=b}}
#
test(S00){a&b:any => {assume{a=b}; a=b}}
test(S00){a&b:any => {a=b; assume{a=b}}}
#Loops following RunComposeStep change: test(S00){a&b:any => {assume{x=a}; x:=a=b}}
test(S00){a&b:any => {x:=a=b; assume{x=a}}}
#
test(S00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume{a=c}; b=d}}

# Synthetic succeeds of effective <decides> assumptions.
test(S00){assume<decides>{1}}
test(F00){assume<decides>{1=2}}
test(D00){a&b:any => assume<decides>{a=b}}
#
test(D00){a&b:any => {a=b; assume<decides>{a=b}}}
test(D00){a&b:any => {assume<decides>{a=b}; a=b}}
test(U00){a&b:any => {x:=a=b; assume<decides>{x=a}}}
##Loops following RunComposeStep change: test(U00){a&b:any => {assume<decides>{x=a}; x:=a=b;}}
#
test(D00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume<decides>{b=c}}}
test(U00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}}}
test(S00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume{b=c}}}
test(S00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; b=d; assume{a=c}}}
test(S00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume{a=c}; b=d}}
test(D00){a&b&c&d:any => {assume<decides>{a=b}; assume<decides>{c=d}; assume{a=c}}}
test(D00){a&b&c:int => if{x:=a=b; assume{x=c}; expect<succeeds>{x=a}}}
test(S00){x&y:any => {assume<succeeds >{x=y}; x=y}}
test(U00){x&y:any => {assume<abstracts>{x=y}; x=y}}
#Loops following RunComposeStep change:  test(S00){a&b:any => {x:=assume<succeeds >{y=a}; y:=assume<succeeds >{x=b}}}
#Loops following RunComposeStep change:  test(D00){a&b:any => {x:=assume<decides  >{y=a}; y:=assume<decides  >{x=b}}}
#Loops following RunComposeStep change:  test(U00){a&b:any => {x:=assume<abstracts>{y=a}; y:=assume<abstracts>{x=b}}}
#Loops following RunComposeStep change:  test(U00){a&b:any => {x:=assume<abstracts>{y=a}; y:=assume<abstracts>{x=b}}}
test(S00){a&b:int => {x:=a; y:=b; assume<succeeds>{x=y}}}
test(S00){a&b:int => {x:=a; y:=b; assume<succeeds>{x=y}; a=b}}
test(S00){a&b:int => {x:=a; y:=b; assume<succeeds>{x=y}; x=y}}
test(S00){a&b:int => if{x:=a; y:=b; assume<succeeds>{x=y}; x=y; expect<succeeds>{x=y}}}
test(S00){a&b:int => {x:=b; assume{x=a}}}
test(S00){a&b:int => {x:=a=b; assume{x=a}}}
test(U00){a&b:any => {x:=a=b; assume<decides>{x=a}}}
test(S00){a&b:int => {x:=a=b; assume{a=b}}}
test(S00){a&b&c:int => if{x:=a=b; assume{x=c}; expect<succeeds>{x=c}}}
test(S00){assume<decides>{1}}
test(S00){a&b:any     => {assume{a=b}; assume<decides>{a=b}}}
test(D00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume<decides>{a=c}}}
test(D00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume<decides>{b=d}}}
test(S00){a&b&c&d:any => {b=c; assume{a=b}; assume{c=d}; assume<decides>{b=d},assume{a=c}}}

# Natural.
test(S00){a&b:int => f(x&y:int):type{x=y} => {f[a,b]; a=b}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {f[a,a]; a=b}}

# Assumption strength.
test(F00){f(x:=0):=x => f[1]}
test(F00){x:int where array{x,2}=array{3,x}}
test(F00){x:int => f():=x=1 => {x=2; f[]}}
test(D00){a&b:int => c:any => f():type{c=a} => {f[]; c=b}}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y) => f[0,3]}

# Consider whether we should flow type constraints similarly to dominators to tighten this slack and prepare for generalizaion.
test(S00){F(a&b:int     )<resolves>:=c:=a=b where expect<succeeds>{int[a]}}
test(S00){F(a&b:int     )<resolves>:=c:=a=b where expect<succeeds>{int[b]}}
test(D00){F(a&b:int     )<resolves>:=c:=a=b where expect<succeeds>{int[c]}} # Slack.
test(D00){F(a:nat)(b:int)<resolves>:=c:=a=b where expect<succeeds>{nat[c]}} # Not slack.
test(D00){F(a:nat)(b:int)(f(x:nat):=x)<resolves>:=c:=a=b where expect<succeeds>{f[a]=f[b]}}

#
# Advanced: Vector data flow modulo effective assumptions.
#

# Effective assumptions on arrays.
test(S00){as:=(:int,:int) => bs:=(11,21) => as(0)=bs(0) => as(0)=bs(0)}
test(D00){as:=(:int,:int) => bs:=(11,21) => as(0)=bs(0) => as(1)=bs(1)}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as=bs}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as(0)=bs(0)}
test(S00){as:=(:int,:int) => bs:=(11,21) => as=bs => as(1)=bs(1)}
test(S00){a:int => as:=(10,20) => bs:=(a,20) => as=bs => as=bs}
test(F00){as:=(10,20) => bs:=(11,20) => as=bs}
test(S00){a:int => (as:=(a,1),bs:=(a,1)) => as=bs}
test(D00){a&b:int => (as:=(a,1),bs:=(b,1)) => as=bs}
test(S00){a&b:int => (as:=(a,1),bs:=(b,1)) => as=bs => as=bs}
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

# Advanced arrays.
test(U00){a:=a => a=(3,4)}
test(S00){a:int => {x:=3,assume{x=a},u:=x=a}}
test(R20){x:=3; assume{x=5}; x=5}
test(R20){x:=2; assume{x=3}}
#Loops following RunComposeStep change: test(S00){assume{x=(_,_,_)},x:=(_,_,_)}
#Loops following RunComposeStep change: test(R20){assume{x=(_,_,_)},x:=(_,_)}
test(U00){a:any => a=(2,3)}
#Loops following RunComposeStep change: test(R20){assume{x=3},x:=4}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as[0]=cs[0]}}}

# Effective array assumptions.
test(S00){as:any => as=array{_}    => as}
test(U00){as:any => as=array{_}    => as[0]=4}
test(F00){as:any => as=array{3}    => as[0]=4}
test(S00){as:any => as=array{3}    => as[0]=3}
test(D00){as:any => as=array{:int} => as[0]=3}
test(S00){as:any => as=array{_} => as[0]}
test(S00){as:any => as=array{_} => as=array{_}} # Now doesn't have dominator; doesn't even get to shootdown.
test(S00){a:any => a=(_,_) => a=(_,_)}
test(S00){a:any => a=(_,_) => a=(_,_)}
test(S00){a:any => a=(3,7) => a[0]}
test(S00){a:any => a=(3,7) => a=(_,_)}
test(S00){a:any => a=as where as:=(a0:=3,a1:=7) => a=(b0:=_,b1:=_)}
test(S00){x:any => assume{x=(3,4)} where x[0]}
test(S00){x:any => y:=x where assume{y=(3,4)} where x[0]}

# Slack in vector data flow.
test(A00){array{_}[0]}
test(S00){array{_}[0]=2}
test(S00){array{_}[0]=3}
test(D00){F(as:=array{a:int}):=as[0]=3}
test(D00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:=cs:=as=bs where expect<succeeds>{as[0]=bs[0]}}
test(D00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:=                expect<succeeds>{as[0]=bs[0]}}
test(D00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:=cs:=as=bs where expect<succeeds>{cs[0]=a}}
test(D00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:=cs:=as=bs where expect<succeeds>{cs[0]=as[0]}}
test(D00){F(as:=array{a:int})(bs:=array{b:int})<resolves>:=                expect<succeeds>{as[0]=bs[0]}}
test(S00){as:=array{3} => as[0]=3}
test(F00){as:=array{3},as[0]=as[0]=4}
test(S00){array{2}}
test(S00){y:=2,y=2}
test(S00){y:=2 where y=2}
test(U00){a&b:any => f(x:=x)<computes>:any => f[a]=f[b]}

# Invisible dominators and projection blocking.
test(U00){as:any => as=array{_} => as[0]=4}
test(U00){a:any => a=(_,_) => a=(4,_)}
test(U00){as:any => as=array{_} => as[0]=4}
test(S00){a:=(3,7) => a=(_,_)}
test(F00){c:any => c=array{8} => c=array{9}}
test(U00){c:any => c=array{9}}
test(F00){c:any => c=3 => c=4}
test(F00){c:any => c=array{8} => c=array{9}}
test(F00){c:any => assume{c=array{8}} where c=array{9}}
test(F00){c:any => c=array{9} where assume{c=array{8}}}
test(F00){c:any => {f()<resolves>:=c=array{8},f[],c=array{9}}}
test(S00){a:any => a=(3,7) => a}
test(F00){a:any => a=(3,7) => a[0]=4}
test(F00){f(:any)<computes>:any => f(2)=(_,_) => f[2]=(_,_,_)}
test(S00){F(as:=array{a:=3 })<resolves>:=bs:=expect<succeeds>{as} where bs[0]=3}
test(D00){F(as:=array{a:int})<resolves>:=bs:=expect<succeeds>{as} where bs[0]=3}
test(U00){F(as:=array{a:any})<resolves>:=bs:=expect<succeeds>{as} where bs[0]=3}
test(U00){a:any => a=array{_} => a[0]=2}
test(S00){a:any => a=array{3} => a[0]=3}
test(F00){a:any => a=array{3} => a[0]=4}

# Eta Conversion.
test(F00){as:=array{a:=8} => c:=as[0]=9}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => {A:=as[0],B:=bs[0],C:=A=B}}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => cs:=as=bs}
test(F00){as:=array{a:=2} => bs:=array{b:=3} => c:=as[0]=bs[0]}
test(D00){as:=array{a:int} => bs:=array{b:int} => {A:=as[0],B:=bs[0],C:=A=B}}
test(D00){as:=array{a:int} => bs:=array{b:int} => {A:=as[0],B:=bs[0],C:=A=B}}
#
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{a=a}}
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{b=b}}
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{c=c}}
test(D00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{a=c}}
test(D00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{b=c}}
test(D00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{a=b}}

# Getting to the heart of directed equality and dominators.
test(S00){F(a&b:int)<resolves>:={p:=(a,b),p=(b,a),expect<succeeds>{p[0]}}}
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{c}}
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{a=a}}
test(D00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{c=a}}
test(S00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{int[a]}}
test(D00){F(a&b:int)<resolves>:=c:=a=b where expect<succeeds>{int[c]}}
test(S00){F(a:=array{:int})(b:=array{:int})<resolves>:=c:=a=b where expect<succeeds>{a=a}}
test(S00){F(a:=array{:int})(b:=array{:int})<resolves>:=c:=a=b where expect<succeeds>{c=c}}
test(D00){F(a:=array{:int})(b:=array{:int})<resolves>:=c:=a=b where expect<succeeds>{c=a}}
test(S00){F(a:=array{:int})(b:=array{:int})<resolves>:=c:=a=b where expect<succeeds>{a=array{:int}}}
test(D00){F(a:=array{:int})(b:=array{:int})<resolves>:=c:=a=b where expect<succeeds>{c=array{:int}}}
test(S00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as=as}}}
test(S00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{cs=cs}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as=cs}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{as=array{:int}}}}
test(D00){F(a&b:int)<resolves>:={as:=array{a},bs:=array{b},cs:=as=bs where expect<succeeds>{cs=array{:int}}}}
test(D00){F(a&b:int)<resolves>:={x:=a,y:=b,z:=x=y where expect<succeeds>{int[x]}}}
test(D00){F(a&b:int)<resolves>:={x:=a,y:=b,z:=x=y where expect<succeeds>{int[z]}}}

# Figure out if tests can expose reduced dominator granularity in arrays.
test(S00){F(a&b:int)<resolves>:=x:=a=b where expect<succeeds>{x}}
test(S00){F(as:=(:int,:int))(bs:=(:int,:int))<resolves>:=xs:=as=bs where expect<succeeds>{xs}}

# Deep array effects.
test(S00){as:=(2,3=4) => as=(_,3)}
test(S00){as:=(2,3=4) => as=(_,4)}
test(S00){as:=(2,3=4) => as=(_,5)}
test(F00){as:=(2,3=4) => as=(7,_)}
test(F00){as:=(2,3=4) => as[0]=7}
test(S00){as:=(2,3=4) => as[1]=3}
test(S00){as:=(2,3=4) => as[1]=5}
test(F00){as:=(2,3=4) => as[5]}

# Strange breakage from Strength equated-initial dominators flow, not fixed.
test(D00){a:int => c:any => c=(a,3) => c=(3,a)}
test(D00){a:int => c:any => assume{c=(a,3)} where c=(3,a)}
test(D00){a:int => c:any => f():type{c=3} => {f[],c=a}}
test(S00){c:any => assume{c=array{3} where c=array{4}}}
test(F00){c:any => f():=c=array{3} => {f[],c=array{4}}}
test(F00){c:any => f():=c=array{8} => {f[],c=array{9}}}
test(F00){c:any => f(7):=c=array{8} => {f[7],c=array{9}}}
test(F00){c:any => f(7):=c=array{8} => f[7] where c=array{9}}
test(F00){c:any => f(7)<computes>:=c=array{8} => {f[7],c=array{9}}}
test(F00){c:any => f(7)<computes>:=c=array{8} => f[7] where c=array{9}}
test(F00){a:any => a=array{7} => a=array{8}}
test(F00){a:any => f():type{a=array{7}} => f[] where a=array{8}}
test(D00){a:int => c:any => f():type{c=array{a}} => {f[],c=array{3}}}
test(D00){a:int => c:any => f():type{c=(a,3)} => {f[],c=(3,a)}}
test(D00){a&b:int => c:any => f():type{c=(a,b)} => {f[],c=(b,a)}}
test(D00){a&b:int => c:any => f():type{c=(a,b)} => {c=(b,a),f[]}}
test(D00){a&b:int => c:any => f():type{c=array{a}} => {c=array{b},f[]}}

# Checking vector flow.
test(U00){F(as:=array{_}   )<resolves>:=bs:=expect<succeeds>{as} where bs[0]=3}
test(F00){F(as:=array{a:=4})<resolves>:=bs:=expect<succeeds>{as} where bs[0]=3}

# Consider future axiomatic semantics.
test(S00){as:=(_,_) => x:=as[7] => as[7]}
test(S00){as:=(_,_) => x:=as[7] => as[7]=x}
test(F00){as:=(_,_) => x:=as[7] => as[8]}
#Loops following RunComposeStep change: test(R20){assume{x=3},x:=4}
#test(F00){as:=(_,_) => x:=as[7] => as[8]=x} # Not sure S00 is wrong.

# Contemplating shape flow.
test(S00){F(a&b:int)<resolves>:=x:=a=b where expect<succeeds>{x}}
test(S00){F(a&b:int)<resolves>:=x:=a=b where expect<succeeds>{any[x]}}
test(D00){F(a&b:int)<resolves>:=x:=a=b where expect<succeeds>{int[x]}}
test(S00){F()<decides>:=x:=:false where expect<succeeds>{x}}
test(S00){F()<decides>:=x:=:false where expect<succeeds>{any[x]}}
test(F00){F()<decides>:=x:=:false where expect<succeeds>{false[x]}}
test(F00){F()<decides>:=x:=:false where expect<succeeds>{int[x]}}
test(S00){F(a:int)(f(:nat):=(3,:int))<resolves>:={xs:=f[a]}}
test(S00){F(a:int)(f(:nat):=(3,:int))<resolves>:={xs:=f[a]; expect<succeeds>{xs[1]}}}
test(D00){F(a:int)(f(:nat):=(3,:int))<resolves>:={xs:=f[a]; expect<succeeds>{int[xs[1]]}}}
test(D00){F(a:int)<resolves>:=cs:=Concatenate[array{array{a}}] where ds:=cs where expect<succeeds>{ds[0]=8; cs=ds}}

# Prior unsoundness.
test(F00){as:=array{2},bs:=array{3},as[0]=bs[0]}
test(F00){as:=array{2},bs:=array{3},as=bs}
test(F00){as:=array{2} where bs:=array{3} where as=bs where as[0]=bs[0]}
test(F00){as:=array{2}; bs:=array{3}; as=bs; as[0]=bs[0]}
test(D00){a&b:int => {as:=array{a},bs:=array{b},cs:=as=bs,as[0]=bs[0]}}
test(F00){array{2}=array{2}=array{3}=array{3}}

test(D00){as:=(:int,:int) => bs:=as => bs=(3,5)}
test(S00){as:=(:int,:int) => bs:=as => bs=(:int,:int)}
test(U00){as:=(:int,:int) => bs:=(:int,:int) => cs:=as => ds:=bs => o:any => {es:=cs=ds,es=(o,o)}}

# Dominates-at. Works without an implementation...
test(S00){F(a:int)<decides>:=cs:=array{5,a}=array{_,6} where expect<succeeds>{cs}}
test(S00){F(a:int)<decides>:=cs:=array{5,a}=array{_,6} where expect<succeeds>{cs[0]}}
test(S00){F(a:int)<decides>:=cs:=array{5,a}=array{_,6} where expect<succeeds>{cs[0]=cs[0]}}
test(S00){F(a:int)<decides>:=cs:=array{5,a}=array{_,6} where expect<succeeds>{cs[0]=5}}
test(S00){F(a:int)<decides>:=cs:=array{_,a}=array{5,6} where expect<succeeds>{cs[0]=5}}
test(S00){F(a:int)<decides>:=cs:=array{_,a}=array{5,6} where expect<succeeds>{cs[1]}}
test(S00){F(a:int)<decides>:=cs:=array{_,a}=array{5,6} where expect<succeeds>{cs[1]=cs[1]}}
test(D00){F(a:int)<decides>:=cs:=array{_,a}=array{5,6} where expect<succeeds>{cs[1]=a}}
test(D00){F(a:int)<decides>:=cs:=array{_,a}=array{5,6} where expect<succeeds>{cs[1]=6}}
test(S00){F(as:=array{6,a:int})<decides>:=cs:=as=array{bs[0],7} where bs:=expect<succeeds>{b:int,cs[0]=6,cs[0]=b}}
test(S00){F(as:=array{6,a:int})<decides>:=cs:=as=array{bs[0],7} where bs:=expect<succeeds>{b:int,cs[1]}}
test(D00){F(as:=array{6,a:int})<decides>:=cs:=as=array{bs[0],7} where bs:=expect<succeeds>{b:int,cs[1]=a}}
test(D00){F(as:=array{6,a:int})<decides>:=cs:=as=array{bs[0],7} where bs:=expect<succeeds>{b:int,cs[1]=7}}

#
# Advanced: Function data flow modulo effective assumptions.
#

# Vector dominators on function.
test(D00){f(x:int):int => f[7]=7}
test(S00){F(as:=array{a:any}):={bs:=as,bs[0]}}
test(U00){F(as:=array{a:any})<resolves>:={bs:=expect<succeeds>{as},bs[0]=3}}
test(S00){f(3):=4 => f[3]}
test(S00){a:comparable => a=a}
test(U00){a&b:any => a=b}
test(D00){a&b:comparable => a=b}
test(U00){f(x:any):any => f[3]=f[3]}
test(D00){f(x:any):comparable => f[3]=4}
test(D00){f(x:any):comparable => f[3]=f[4]}
test(D00){f(x:any):comparable => f[3]=f[3]}
test(S00){F(f(x:any):any)<resolves>:=f[3]=4}
test(U00){F(f(x:any):any)<resolves>:=g:=expect<succeeds>{f} where g[3]=4}
test(D00){F(f(x:any):int)<resolves>:=g:=expect<succeeds>{f} where g[3]=4}
test(D00){F(f(x:comparable):comparable)<resolves>:=g:=expect<succeeds>{f} where g[3]=4}
test(S00){comparable[123]}
test(S00){F(a:any):={f(x:any):=a,f[3]}}
test(U00){F(a:any)<resolves>:={f(x:any)<computes>:=a; g:=expect<succeeds>{f}; g[3]=4}}
test(D00){F(a:comparable)<resolves>:={f(x:comparable)<computes>:=a; g:=expect<succeeds>{f}; g[3]=4}}
test(D00){F(a:comparable):={bs:=array{:comparable} => a=bs[0]}}
test(D00){a&b:comparable => f[]=a where f():=b}
test(D00){a&b:int => a=b}
test(D00){f(x:any):int => f[3]=4}
test(D00){a:comparable => f():comparable => f[]=a}

# Domains.
test(S00){F(x&y:int)(f(:int)<computes>:int)(f[x])<resolves>:={a:=x=y,expect<succeeds>{f[x]}}}
test(D00){F(x&y:int)(f(:int)<computes>:int)(f[x])<resolves>:={a:=x=y,expect<succeeds>{f[a]}}}
test(S00){F(x&y:int)(f(:int):int)(f[x])<resolves>:={a:=x=y,expect<succeeds>{f[x]}}}
test(D00){F(x&y:int)(f(:int):int)(f[x])<resolves>:={a:=x=y,expect<succeeds>{f[a]}}}

#
# Advanced: Iterate exploration.
# Inference on iterate (if/for) with domain decidable but not known to be succeeds/fails requires
# that we test both prospects in parallel, while ensuring eventual confluence.
#

# Conditions and reachability, synchronous and asynchronous.
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
test(F00){x  :int => f():type{x=1} => {    if(x=2){:false}else{}    }}
test(S00){x  :int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}                }}
test(S00){x&y:int => f():type{x=y} => {f[],if(x=y){}                }}
test(S00){x&y:int => f():type{x=y} => {    if(x=y){}            ,f[]}}
test(F00){x&y:int => f():type{x=y} => {    if(x=y){}else{:false}    }}
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
test(U00){f(x:any):=if(x=2){3}else{4}}
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

# Conditions Triggering Refork.
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=10 where B:=20}
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=11 where B:=20}
#test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=10 where B:=21}
test(S00){a&b:int => if(a=10){}else{a=A} where if(b=20){}else{b=B} where A:=11 where B:=21}

# Conditions requiring control flow exploration.
test(S00){x&y:int => if{x=y}}

# Fails-cancellation.
#test(S00){a:int => if(a=1){a=1}else{not a=1}} # works withh fails-cancellation
test(D00){a:int => if(a=1){a=1}else{a=1}}  # breaks withh fails-cancellation

# Works, but why is performance not greatly improved by suspending DecidesFork?
test(S00){as:=(:int,:int,:int,:int,:int) => for{a where i->a:as,a=i}}

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
test(S00){x:int => f():type{1|2} => if(x=f()){1}else{2}}

# Condition transitions must be monotonic.
test(S00){F(a:any)(b:int):={if{b=3}}}
test(U00){F(a:any)(b:int):={x:=b,if{x=3},x=a}}
test(U00){F(a:any)(b:int):={x:=a,if{x=3},x=b}}
test(S00){F(a:any)(b:int)<resolves>:={b=a,if{b=3}}}
test(S00){F(a:any)(b:int)<resolves>:={if{b=3},b=a}}

# Interesting conditions.
test(D00){a:int => if(a=1){a=1}else{a=1}}
test(D00){a:int => if(a=1){a=1}else{a=2}}
test(F00){a:int => if(a=1){a=1}else{not a=2}}
#test(S00){a:int => if(a=1){a=1}else{not a=1}} # failure cancellation problem?

# New instances of iteration unsoundness.
test(A00){f(x:int):int => {first{int[x]},x:=f[x]}}
test(F00){a:int => f(:nat):int => {x:=if(f[a]){1}else{2},nat[a],x=1}}

# Functions with conditions.
test(S00){f(x:int where if(x=2){3}else{4}):=x; f[0]}
test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,4]}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,3]}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,4]}
test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[0,:int]}
#WTF test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,:int]}
#broken by shape-blocking shape flow WTF: test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),f[2,3]}
test(S00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => if{f[a,3]}}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => f[a,3]}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y),a:int => f[a,4]}
test(F00){f(x:int,y:=if(x=2){3}else{4}):=(x,y) => f[0,3]}

# Old breakage.
test(S00){f(1|2):int => for{f[x:int]=x}} # why not instantiating x?
test(A00){f(:int):int => for{f[x:int]=x}}

# Functions returning conditions.
test(S00){a:int => f(x:int):int => if{f[a]=1}}
test(S00){a:int => f(x:nat):int => if{f[a]}}
test(S00){a:int => f(x:nat):int => if{f[0]=1}}
test(S00){a:int => f(7    ):int => if{f[a]}}
test(S00){a:int => f(7    ):int => if{f[7]=1}}
test(S00){a:int => f(7    ):int => if{f[a]=1}}
test(S00){a:int => f(x:nat):int => if{f[a]=1}}
test(S00){a&b:int => if(a=b){a=b}}

# Condition freedom.
test(S00){a:int => {x:=a; x=7 => x=7}}
test(S00){a:int => {x:=a; x=a => x=a}}
test(S00){a:int => {x:=7; if(x=a){x=a}}}
test(S00){a:int => {x:=a; if(x=7){x=7}}}
test(S00){a&b:int => {x:=a; if(x=b){x=b}}}
test(S00){a&b&c:int => {x:=a; if(x=b){x=a; x=b}}}
test(S00){a:int => if(nat[a]) then nat[a]}
test(S00){a:int => {x:=a,if(nat[x]) then nat[x]}}
test(D00){a&b&c:int => {x:=a,if(x=b){x=a,x=b,x=c}}}
test(S00){a&b:int => {x:=a,if(x=a){1}else{2}}}      # succeeds
test(S00){a&b:int => {if(x=a){1}else{2},x:=a}}      # waits for decidable, succeeds
test(S00){a&b:int => {x:=b,if(x=a){a=b}else{2}}}    # decidable; assumes and succeeds
test(S00){a&b:int => {if(x=a){a=b}else{2},x:=b}}    # waits for decidable; assumes and succeeds; succeeds
test(D00){a&b:int => {x:=a,if(x=a){1}else{2},x=b}}  # succeeds, may fail
test(D00){a&b:int => {x:=b,if(x=a){1}else{2},x=a}}  # decidable; assumes and succeeds; may fail

# Conditions with choice.
test(S00){if(a:=1|2){a=1}}
test(F00){if(a:=1|2){a=2}}
test(S00){x:int => if(x=1|2){if{x=1}else{x=2}}}
test(S00){x:int => if(x=1|2){x=1 or x=2}}
test(S00){x:int => if(x=1|2){x=1 or x=3 or x=2}}
#test(F00){x:int => if(x=1|3|2){x=1 or x=2}}

# Exploration narrowing. Previously affected by premature <unifies> freeness. Top-level.
test(S00){x:int=0; if(x=0) then 789 else 789}
test(F00){x:int=0; if(x=0) then 7=9 else 789}
test(F00){x:int=0; if(x=0) then 789 else 7=9}
#
test(S00){x:int=0; assume{x=0}; if(x=0) then 789 else 789}
test(F00){x:int=0; assume{x=0}; if(x=0) then 7=9 else 789}
test(S00){x:int=0; assume{x=0}; if(x=0) then 789 else 7=9}
#
test(S00){x:int=1; assume{x=1}; if(x=0) then 789 else 789}
test(S00){x:int=1; assume{x=1}; if(x=0) then 7=9 else 789}
test(F00){x:int=1; assume{x=1}; if(x=0) then 789 else 7=9}
#
test(S00){x:int=0; if(x=0) then 789 else 789; assume{x=0}}
test(F00){x:int=0; if(x=0) then 7=9 else 789; assume{x=0}}
test(S00){x:int=0; if(x=0) then 789 else 7=9; assume{x=0}}
#
test(S00){x:int=1; if(x=0) then 789 else 789; assume{x=1}}
test(S00){x:int=1; if(x=0) then 7=9 else 789; assume{x=1}}
test(F00){x:int=1; if(x=0) then 789 else 7=9; assume{x=1}}
#
test(F00){x:int=0; if(x=0){1}else if(x=1){2}else{3}=2; assume{x=0}}
test(F00){x:int=1; if(x=0){1}else if(x=1){2}else{3}=1; assume{x=1}}
test(F00){x:int=2; if(x=0){1}else if(x=1){2}else{3}=9; assume{x=2}}
#
test(F00){x:int=0; if(x=0){1}else if(x=1){2}else{3}=3; assume{x=0}}
test(S00){x:int=1; if(x=0){1}else if(x=1){2}else{3}=2; assume{x=1}}
test(F00){x:int=1; if(x=0){1}else if(x=1){2}else{3}=3; assume{x=1}}
test(F00){x:int=2; if(x=0){1}else if(x=1){2}else{3}=2; assume{x=2}}
test(S00){x:int=2; if(x=0){1}else if(x=1){2}else{3}=3; assume{x=2}}
#
test(F00){x:int=7; if(x=6){9}else{3}=9; assume{x=7}}
test(F00){x:int=7; if(x=6){9}else{3}=0; assume{x=7}}

# Lambda-nested simple.
test(S00){x:int => if(x=0) then 789 else 789}
test(F00){x:int => if(x=0) then 7=9 else 789}
test(F00){x:int => if(x=0) then 789 else 7=9}
#
test(S00){x:int => {assume{x=0}; if(x=0) then 789 else 789}}
test(F00){x:int => {assume{x=0}; if(x=0) then 7=9 else 789}}
test(S00){x:int => {assume{x=0}; if(x=0) then 789 else 7=9}}
#
test(S00){x:int => {assume{x=1}; if(x=0) then 789 else 789}}
test(S00){x:int => {assume{x=1}; if(x=0) then 7=9 else 789}}
test(F00){x:int => {assume{x=1}; if(x=0) then 789 else 7=9}}
#
test(S00){x:int => {if(x=0) then 789 else 789; assume{x=0}}}
test(F00){x:int => {if(x=0) then 7=9 else 789; assume{x=0}}}
test(S00){x:int => {if(x=0) then 789 else 7=9; assume{x=0}}}
#
test(S00){x:int => {if(x=0) then 789 else 789; assume{x=1}}}
test(S00){x:int => {if(x=0) then 7=9 else 789; assume{x=1}}}
test(F00){x:int => {if(x=0) then 789 else 7=9; assume{x=1}}}

# Lambda-nested exploration narrowing sans assume.
test(F00){x:int => f():type{x=0} => {f[]; if(x=0) then 7=9 else 789}}
test(S00){x:int => f():type{x=0} => {f[]; if(x=0) then 789 else 7=9}}
#
test(S00){x:int => f():type{x=1} => {f[]; if(x=0) then 789 else 789}}
test(S00){x:int => f():type{x=1} => {f[]; if(x=0) then 7=9 else 789}}
test(F00){x:int => f():type{x=1} => {f[]; if(x=0) then 789 else 7=9}}
#
test(S00){x:int => f():type{x=0} => {if(x=0) then 789 else 789; f[]}}
test(F00){x:int => f():type{x=0} => {if(x=0) then 7=9 else 789; f[]}}
test(S00){x:int => f():type{x=0} => {if(x=0) then 789 else 7=9; f[]}}
#
test(S00){x:int => f():type{x=1} => {if(x=0) then 789 else 789; f[]}}
test(S00){x:int => f():type{x=1} => {if(x=0) then 7=9 else 789; f[]}}
test(F00){x:int => f():type{x=1} => {if(x=0) then 789 else 7=9; f[]}}
#
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=2; assume{x=0}}}
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=1; assume{x=1}}}
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=9; assume{x=2}}}
#
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=3; assume{x=0}}}
test(S00){x:int => {if(x=0){1}else if(x=1){2}else{3}=2; assume{x=1}}}
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=3; assume{x=1}}}
test(F00){x:int => {if(x=0){1}else if(x=1){2}else{3}=2; assume{x=2}}}
test(S00){x:int => {if(x=0){1}else if(x=1){2}else{3}=3; assume{x=2}}}
#
test(F00){x:int => {if(x=6){9}else{3}=9; assume{x=7}}}
test(F00){x:int => {if(x=6){9}else{3}=0; assume{x=7}}}

# Conditions and assumptions.
test(D00){a:int => {a=3,if{a=3}}}
test(D00){a:int => {if{a=3},a=3}}
test(S00){a&b:int => f():type{a=b} => {if{a=b} else f[]}}
test(S00){a&b:int => f():type{a=b} => {if{a=b} else f[],a=b}}
test(S00){a&b:int => f():type{a=b} => {a=b,if{a=b} else f[]}}
test(S00){a&b:int => f():type{a=b} => {if{a=b,f[]} else f[],a=b}}
test(S00){a&b:int => f():type{a=b} => {if{f[],a=b} else f[],a=b}}
test(S00){a&b:int => f():type{a=b} => if{a=b,f[]}}

# Decidable conditions that later succeed.
test(S00){a:int => f():type{a=3} => {if(a=3) then 123 else :false, f()}}
test(F00){a:int => f():type{a=3} => {if(a=4) then 123 else :false, f()}}
test(F00){a:int => f():type{a=3} => {if(a=3) then :false else 123, f()}}
test(S00){a:int => f():type{a=3} => {if(a=4) then :false else 123, f()}}

# Dominators and escalation avoidance.
test(S00){a&b:int => if{x:=a=b,expect<succeeds>{x}}}
test(D00){a&b:int => if{x:=a=b,expect<succeeds>{int[x]}}}
test(D00){a&b:int => f(a=b):=a=b => if{x:=a=b,expect<succeeds>{f[x]}}}
test(D00){a&b:int => f(a=b):=a=b => {x:=a=b,f[x]}}
test(D00){a&b:int => f(x:int):=int[x] => if{x:=a=b,expect<succeeds>{f[x]}}}

# Conditions lacking runtime semantics.
test(A00){x:int,if(x=3) then x=3 else x=4}
test(A00){x:int,if(int[x]) then x=3 else x=4}
test(S00){f():type{x:int,if(x=3) then x=3 else x=4} => f[]}
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

# Various.
test(S00){x:int => f():type{x=1} => {    if(x=2){:false}else{},f[]}}
test(F00){x:int => f():type{x=1} => {x=2,f[]}}

# Context promotion recognition.
test(S00){x:int => f():type{x=2} => {first{x=2},f[]}}
test(S00){x:int => f():type{x=2} => {first{x=2},if{f[]}}}

# Function range verification failure.
test(S00){if(f():=1){1}else{2}=1}
test(F00){if(f():false){1}else{2}}

# Interesting conditions.
test(S00){a:int => g(:nat):int => f(x:int):int => if{f[g[a]]}}

# Verifying all branches of conditions and choices.
# We don't do this yet!
#test(N00){if(1){2}else{x}}
#test(N00){if(1=2){x}else{3}}
#test(N00){if(1|x){2}}

#
# Advanced: Nuances of desugaring.
#

# Grammar (WhereIn).
test(S00){f():=x where x:=3}
test(S00){x:array{3} where 3}
test(S00){i->x:int where i=3}
test(S00){f():=a:=2 where a}

# Input desugaring.
test(D00){f(a:int)<decides>:={b:=type{3}[a]; expect<succeeds>{b=3}}}

# Native type_fx_abstraction, abstraction_type.
test(S00){int}
test(S00){succeeds}
test(A00){1<2>}        # Want better error.
test(A00){1<succeeds>} # Want better error.
test(A00){int<3>}      # Want better error.
test(S00){int<succeeds>}
test(C00){int<succeeds>[3]}
test(C00){int<succeeds>['3']}
test(S00){abstraction_type[int]}
test(S00){abstraction_type[type{3}<succeeds>]}
test(S00){abstraction_type[type{3}<succeeds>][3]}
test(F00){abstraction_type[type{3}<succeeds>]['3']}
test(S00){:type{3}<succeeds>}
test(S00){x:int<succeeds>; x=3}
test(S00){x:int<succeeds> => int[x]}
test(S00){f():int<computes> => f[]}
test(S00){f():int<succeeds> => int[f[]]}

#
# Advanced: 'iterate' with unknown and unbounded number of iterations.
#

# We haven't implemented support for this feature yet!
#test(S00){f(xs:[]int):=for(x:xs){x+1},f(3,5,7)=(4,6,8)}
#test(S00){Concatenate(xs:[]any,ys:[]any):=for{xs?|ys?},Concatenate((1,2),(3,4))=(1,2,3,4)}
#test(S00){n:nat => for(x:0..n,y=:10|20){x+y}} # (x+10,x+20)+..
#test(S00){n:nat => for(y:=10|20,x:0..n){x+y}} # (x+10,..)+(x+20,..)

#
# Advanced: Modus Ponens and Implication Strength.
# When calling a function f[x] producing a result implication :t, we must assume :t no
# more strongly than the inferred strength of the f's domain test against x.
#

# Modus Ponens timing sensitivity.
test(S00){a:any => f(x&y:any):type{a=3} => if{a,3,f[a,3]}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[a,b],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[a,b],x=y}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],x=y}}
test(D00){a&b:int => f(x&y:int):type{x=y} => if{x:=a,y:=b,f[x,y],x=y,expect<succeeds>{x=y}}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,a=b,f[x,y]}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,x=y,f[x,y]}}
test(D00){a&b:int => f(x&y:int):type{x=y} => if{x:=a,y:=b,x=y,f[x,y],expect<succeeds>{x=y}}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],a=b}}
test(D00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}
test(D00){a&b:int => f(x&y:nat):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}
test(U00){a&b:any => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,x],x=y}}

# Lambda weakening.
test(S00){a:int => f():=(a=3,:type{a=3}) => 3}
test(F00){f(x:=3):type{x=3} => f[4]}
test(S00){f(x:int):=(x=3,:type{x=3}) => 4}
test(D00){f(x:int):=x=3 => 4}
test(S00){f(x:int):type<decides>{x=3} => 4} # Because op::in intersects ImpliedFx.
#
test(S00){f(x:int):=(    :type{x=3}) => 4}
test(S00){f(x:int):=(x=3,:type{x=3}) => 4}
test(S00){f(x:int):=(x=3,:type{x=3}<succeeds>) => 4}
test(D00){f(x:int)<decides>:=expect<succeeds>{x=3,:type{x=3}          } => 4}
#test(D00){f(x:int)<decides>:=expect<succeeds>{    :type{x=3}          } => 4} # Result of 'expect' narrowing allowed effects.
#test(D00){f(x:int)<decides>:=expect<succeeds>{    :type{x=3}<decides >} => 4} # Result of 'expect' narrowing allowed effects.
test(D00){f(x:int)<decides>:=expect<succeeds>{x=3,:type{x=3}<decides >} => 4}
test(S00){f(x:int)<decides>:=expect<succeeds>{    :type{x=3}<succeeds>} => 4}
test(S00){f(x:int)<decides>:=expect<succeeds>{x=3,:type{x=3}<succeeds>} => 4}
#
test(S00){f(x:int):=(    :type{x=3}          ) => 4}
test(S00){f(x:int):=(x=3,:type{x=3}          ) => 4} # Because we LambdaFx mask the whole range.
test(S00){f(x:int):=(    :type{x=3}<decides >) => 4} # Because op::in intersects ImpliedFx.
test(S00){f(x:int):=(x=3,:type{x=3}<decides >) => 4} # Because op::in intersects ImpliedFx.
#
test(S00){a:nat => f(x:nat)<decides>:type{nat[a]}<succeeds> => f[a]}
test(D00){a:int => f(x:nat)<decides>:type{nat[a]}<succeeds> => f[a]}
test(S00){a:nat => f(x:int)<decides>:=let(nat[x]) do :type{nat[a]}<succeeds> => f[a]}
test(S00){a:int => f(x:int)<decides>:=let(nat[x]) do :type{nat[a]}<succeeds> => f[a]}

# Implication stacking. Shows f[a] strength is the weaker of f imply strength and f[a] domain strength.
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
test(U00){a:int => b&c:any => f(:nat):=b=c => 7}
test(D00){a:int => b&c:any => f(:nat):type{b=c} => f[a]}
test(D00){a:int => b&c:any => f(:nat):type{b=c} => {f[a],b=c}}
test(U00){a:any => b&c:any => f(:nat):=b=c => f[a]}
test(D00){a&b&c:int=>f(x:nat)():=nat[a]=>f[a][]}
test(D00){a:int => f(0)():int => f[a]}
test(S00){a:int => f(0)():int => f[a] => 4}
test(S00){a:int => f(0)():int => f[a][] => 4}
test(D00){a:int => f(b:nat):=nat[b] => f[a]}
test(D00){a:int => f(:nat):=nat[a] => f[a]}
test(D00){a:int => f(:int):=nat[a] => f[a]}
test(S00){a:int => f(:int):type{nat[a]} => f[a]}
test(D00){a:int => f(6)():=7 => f[a]}
test(D00){a:int => f(b:nat)():=nat[b] => f[a]}
test(D00){a:int => f(b:nat)():=nat[b] => f[a][]}
test(D00){a:int => f(:nat)():type{a=8} => {r:=f[a]; r[]}}
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
test(D00){a&b:int => f(a):type{a=b} => {f[x],x:=b}}
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
test(D00){a&b&c&d:int => f(a):type{c=d} => g(c):=a=b => {f[x],g[d],x:=b}}
test(D00){a:int => {x:=x,x=a,x=3}}
test(D00){a:int => {x:=x,x=3,x=a}}
test(D00){a:int => {x:int,x=a,x=3}}
test(D00){a:int => {x:int,x=3,x=a}}

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

# Nested functions and available implications.
test(R00){assume{x:int}}
test(S00){assume{f():int}}
test(S00){assume{f()():int}}
test(S00){f():int => f[]}
test(S00){f()():=3 => 123}
test(S00){f()():int => 123}
test(S00){f()():=3 => f[]}
test(S00){f(5)(6):=7 => f[5]}
test(S00){f()():int => f[]}
test(S00){f()<decides>():int => f[]}
test(S00){a:int => f(:int)():type{nat[a]} => f[a]}
test(D00){a:int => f(:nat)():type{nat[a]} => f[a]}
test(D00){a:int => f(:nat)<decides>():type{nat[a]} => f[a]}
test(S00){a:int => f(:int):type{g():type{nat[a]}} => f[a]}
test(D00){a:int => f(:nat):type{g():type{nat[a]}} => f[a]}

# Was formerly unsound when treating parent iterate assumptions as succeeds.
test(S00){F(a:int)(a=3):=x:type{a=3}=a}
test(V10){F(a:int):=x:type{a=3}=a}
test(V10){F(a:int):=x:type{a=3}=3}
test(V10){F(a:int):=x:type{a=3}=a=3}
test(V10){F(a:int):=x:type{a=3}=x}
test(V10){F(a:int)<resolves>:={a=3, expect<succeeds>{f():type{a=3}=a=3}}}
test(V10){F(a:int):=if{a=3,f():type{a=3}=a=3}}
test(V10){F(a:int):={a=3, f():type{a=3}=a=3, f[]}}
test(V10){F(a:int):=if{a=3, f():type{a=3}=a=3, f[]}}
test(S00){type{x:int,if{x=2}}}

# Proper explore.UnpromotableNegotiable effects propagation.
test(S00){f(g(:int)<decides>:int):int => f[false]}
test(U00){f(g(:int)<succeeds>:int):int => f[false]}
#test(S00){f(g(:int)<fails>:int):int => f[false]}

# Have we introduced unsoundness by masking out h but allowing granular :t<x>? Seems not.
test(S00){f(g(:any)<decides >:any):=3 => f[false]}
test(U00){f(g(:any)<succeeds>:any):=3 => f[false]}

# More.
test(D00){a&b:int => f(a=b):=3 => f[a=b]}
test(P00){a&b:int => f(a=b):=3 => f(a=b)}
test(D00){a&b:int => a=b where f():=a=b}
test(S00){a&b:int => f(:any):type{a=b} => f[a=b]}
test(D00){a&b:int => f(a=b):type{a=b} => f[a=b]}
test(F00){f(:any):false => x:false where f[x]}

# Modus ponens and circularity.
test(S00){f(a:any):any => x:=f[2]}
test(S00){f(a:any):any => x:=f(2)}
test(A00){f(a:any):any => x:=f[x]}
test(P00){f(a:any):any => x:=f(x)}
test(U00){f(a:any):any => x:=f[x]=2}
test(S00){f(a:any):=a  => x:=f[x]=2}
test(S00){f(a:any):=(3,a) => x:=f(2)}
#
test(S00){f(a:int):int => x:=f[2]}
test(S00){f(a:int):int => x:=f(2)}
test(A00){f(a:int):int => x:=f[x]}
test(P00){f(a:int):int => x:=f(x)}
test(D00){f(a:int):int => x:=f[x]=2}
test(S00){f(a:int):=a  => x:=f[x]=2}
#
test(A00){f(a:any):=(3,a) => x:=f[x]}
test(P00){f(a:any):=(3,a) => x:=f(x)}
test(P00){f(a:int):=(3,a) => x:=f(x)}
test(S00){f(a:int):=(3,a) => x:=f(2)}
#
test(S00){f(a:comparable):comparable => x:=f[2]}
test(S00){f(a:comparable):comparable => x:=f(2)}
test(A00){f(a:comparable):comparable => x:=f[x]}
test(P00){f(a:comparable):comparable => x:=f(x)}
test(D00){f(a:comparable):comparable => x:=f[x]=2}
test(S00){f(a:comparable):=a         => x:=f[x]=2}
test(S00){f(a:comparable):=(3,a) => x:=f(2)}
#loops test(P00){f(a:comparable):=(3,a) => x:=f(x)}
#loops test(P00){f(a:comparable):comparable=(3,a) => x:=f(x)}

# Lambda weakening:
test(S00){F(a:int    )(f():=(:type{a=3},a=3)         ):=3}
test(S00){F(a:int    )(f():=(:type{a=3},a=3)         ):=f[]}
test(S00){F(a&b&c:int)(f():=:type{a where c=3}       ):={f[]; c=3}}
test(D00){F(a&b&c:int)(f():=a where :type{c=3}       ):={f[]; c=3}}
test(S00){F(a&b&c:int)(f():type{a where c=3}         )<resolves>:={f[]=b;   expect<succeeds>{c=3}}}
test(S00){F(a&b&c:int)(f():=:type{a where c=3}       )<resolves>:={f[]=b;   expect<succeeds>{c=3}}}
test(D00){F(a&b&c:int)(f():=a where :type{c=3}       )<resolves>:={f[]=b;   expect<succeeds>{c=3}}}
test(S00){F(a&b:int  )(x:int)(f(:int):type{a=b}      )<resolves>:={y:=f[x]; expect<succeeds>{y=a}}}
test(D00){F(a&b:int  )(x:int)(f(:nat):type{a=b}      )<resolves>:={y:=f[x]; expect<succeeds>{y=a}}}
test(D00){F(a:int    )(f(x:nat)<decides>:type{nat[a]}):=f[a]}
test(D00){F(a:int    )(f(x:int)<decides>:=let(nat[x] ) do :type{nat[a]}):=f[a]}
test(D00){F(a:int)(f(x:nat)<decides>:type{nat[a]}<succeeds>):=f[a]}
test(S00){F(a:int)(f(x:int)<decides>:=let(nat[x]) do :type{nat[a]}<succeeds>):=f[a]}

# Implication order.
test(S00){f()<decides>:int => g():type{f[]} => g[]}

# Interesting effective reachability tests.
test(D00){p&q:int => f(:int):int => f[p]=3 => f[q]=5 => p=q} # Could be F00. Do we want that kind of inference?
test(F00){as:=(:int,:char) => int(as[0]) where 0=1}

# Contingency basics.
test(D00){x:=assume{:int},y:=assume{:int},x=y}
test(R00){x:=assume{:int},int[x]}
test(D00){x:=assume{:int},x=3}
test(D00){x:=assume{:int},nat[x]}
test(D00){x:int => nat[x]}
test(U00){x:=assume{:any},y:=assume{:int},x=y}
test(U00){x:=assume{:int},y:=assume{:any},x=y}
test(U00){x:=assume{:any},x=3}

# Consider information parity and soundness.
test(S00){a:int => b:any => f(:int):type{a=b} => {f[a],a=b}}
test(S00){a:int => b:any => f(:int):type{a=b} => {a=b,f[a]}}
test(S00){a:int => b:any => f(a   ):type{a=b} => {f[a],a=b}}
test(S00){a:int => b:any => f(a   ):type{a=b} => {a=b,f[a]}}
test(D00){a:int => b:any => f(:int):type{a=b} => f[a=b]}
test(D00){a:int => b:any => f(a):type{a=b} => f[a=b]}
test(S00){b:any => f(x:any):type{int[x]} => {f[b],int[b]}}
test(U00){b:any => f(x:int):=int[x] => f[b]}

# Domain decidability.
test(D00){a&b:int => f(a):=9 => {a=b,f[b]}}

# Dependence on parent domain resolves effect.
test(P00){f(x:int):=x => f(:int)=3}
test(S00){a&b:any => f(:any)<computes>:any => c:=f[a] => d:=f[b] => a=b => c=d}
test(A00){f(:int):int => {x:=f[x],if(int[x]) then x=3}}

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

# Contingent decidability.
test(S00){a&b&c:int => f(x:int):type{a=b} => f[3]}
test(D00){a&b&c:int => f(c    ):type{a=b} => f[3]}
test(S00){a&b:int => f(x&y:any):type{x=y} => {x:=a,y:=b,f[x,y]}}
test(S00){a&b&c:int => f(:int):type{a=b} => {x:=a,y:=b,f[3]}}
test(D00){f(:int):int => f[3]=f[4]}

# Fixed point convergence.
test(S00){a&b:int => f(a):type{a=b} => {f[a],a=b}}
test(S00){a&b:int => f(x&y:any):=0 => {x:=a,y:=b,f[x,y]}}
test(S00){a&b:int => f(x&y:any):=x => {x:=a,y:=b,f[x,y]}}
test(S00){a&b:int => f(x&y:any):=y => {x:=a,y:=b,f[x,y]}}
test(S00){a:int => f(x:any):type{a=3} => f[a]}
test(D00){a:int => f(x:int):int => f[a]=3}
test(D00){a&b:int => f(a):type{a=b} => {f[x],x:=b}}
test(D00){a&b:int => f(a):type{a=b} => {x:=b,f[x]}}
test(S00){a:int => f(x:nat):int => if{f[a]=1}}
test(S00){a:int => f(x:int):type{x} => f[a]=a}
test(D00){a:int => f(x:int):=x => f[a]=3}
test(S00){a:int => f(x:any):type{x=3} => f[a]}
test(S00){a&b:int => f(x&y:any):type{x=y} => {x:=a,y:=b,f[x,y]}}
test(D00){a&b:int => f(x:int):=x => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => {a=b,f[a]=f[b]}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y]}}

test(S00){a:int => b:int => f():type{a=b} => {f[],a=b}}
test(S00){a&b:int => f():type{a=b} => {f[],a=b}}

# Representative tests that were previously broken by array matching.
test(D00){a&b:int => f(x:int)<computes>:=x => f[a]=f[b]}
test(D00){a&b:int => f(x:int):=x => f[a]=f[b]}
test(S00){F(a:int)(f(x:int):type{x=3}):=f[a]}
test(D00){F(a:int)<resolves>:={x:=expect<succeeds>{int[a]},x=3}}
test(S00){a&b:int => f(x&y:int):type{x=y} => f[a,b]}
test(S00){a&b:int => f(x&y:int):type{x=y} => {a=b,f[a,b]}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {f[a,b],a=b}}
test(S00){a&b:int => f(x&y:int):type{x=y} => {x:=a,y:=b,f[x,y],a=b}}
test(S00){F(a:int)(b:any)(f(x:int):=int[x]):=expect<succeeds>{f[a]}}
test(U00){F(a:int)(b:any)(f(x:int):=int[x]):=expect<succeeds>{f[b]}}
test(S00){F(a:int)(b:any)(f(x:int):=int[x])<resolves>:=a=b where expect<succeeds>{f[a]}}
test(U00){F(a:int)(b:any)(f(x:int):=int[x])<resolves>:=a=b where expect<succeeds>{f[b]}}
test(D00){a&b&c:int => if{x:=a=b,assume{x=c},expect<succeeds>{x=a}}} # Consider.
test(S00){f()():int => 123}
test(S00){f()<decides>():int => 123}
test(S00){x:int=3 => x}
test(S00){f():int=3 => f}
test(S00){f()():int=3 => f}
test(S00){f()<decides>():int=3 => f}
test(D00){a&b&c&d:any => {b=c,assume{a=b},assume{c=d},assume<decides>{a=c}}}

# Treatment of <abstracts> in non-negotiable contexts.
test(A00){f(:int):int => x:=f[x]}
test(A00){f(:int):int => {x:=f[x]; if{int[x]}}}
test(A00){f(:int):int => {x:=f[x]; if(x=3) then xyzzy}}
test(A00){f(:int):int => {x:=f[x]; if(x=3) then x=3}}
test(A00){f(:int):int => {x:=f[x]; if(int[x]) then xyzzy}} # Condition stuck, so N00 not reached because con.
test(A00){f(:int):int => {x:=f[x]; if(int[x]) then assume{x=3}}}
test(A00){f(:int):int => {x:=f[x]; if(int[x]) then x=3}}

# Exploring whether there should be abstracts rep-flow.
test(S00){x:any => x+1 where assume{x=3}}
test(S00){x:any; x+1   where assume{x=3}}

# Flow variable strength.
test(S00){a&b:int => f(:any):type{a=b} => f[a=b]} # D00 with variable strength.
test(F00){f(:any):false => x:false where f[x]}    # F00 with variable strength.
test(S00){a&b:int => assume{a=b}}
test(S00){F(a:int)<resolves>:=x:=a=3 where expect<succeeds>{assume{x}}}
test(S00){type{x:=3,y:false where assume{x=y}}}
test(S00){type{x:=3,y:=(:false where x=y        ) where x}}
test(S00){type{x:=3,y:=(:false where assume{x=y}) where x}}

# Messing with AvailableImplication.
test(S00){f():=(i:int,i) => 4}
test(D00){f():=(i:int,i=3) => 4}
test(S00){f():=(i:int,:type{i=3}) => 4}
test(S00){f():type{i:int,i=3} => 4}

# Fixing HoldFx strangeness so domain :int isn't a weird special case.
test(S00){a:int => f(3):int => if{f[a]}}
test(S00){a:int => f(:nat):int => if{f[3]}}
test(S00){a:int => f(:nat):int => if{f[a]}}

# Enforcing <abstracts>.
test(A00){F(f(x:int):int)<resolves>:={x:=f[x]; expect<succeeds>{if{x=3}}}}

# Assuming input test (and, if function, capturing function strength).
test(D00){a:int => f(3)():=4 => f[a]}              # Input test assumes as strong as function Abstraction.
test(S00){g(f(3):=4):=5, a:int => g[f(a):=4] => 5} # Input test assumes as strong as caller.
test(U00){g(f(3):=4):=5, a:int => g[f(a):=4]}      # Input test doesn't assume otherwise.

# Modus ponens + computes.
test(U00){f()<computes>:=a=b where a&b:int => f[]=f[]}
test(S00){f()<computes>:type{a=b where a&b:int} => f[]=f[]}
test(D00){f():type{a=b where a&b:int} => f[]=f[]}
test(U00){c:int => f(c):={a=b where a:int,b:int} => f[3]}

# Ranjit Jhala's question on the need for Modus Ponens instead of wait-for-domain-success-then-run-range,
# followed by Andy Sonnenberg's follow-up examples.
test(S00){g(f(x:int):=x):=f[_]=3}
test(S00){g(f(x:int):type{3}):=y:=f[y]}
test(S00){g(f(x:int)<decides>:=x=3):=f[_]}
test(S00){g(f(x:int):type{3}):=y:=f[y]; h(x:int):=if(x=2) then 3 else 3}
test(U00){Y:=if(Y=2) then Y+1 else 3}
test(S00){Y:type{3}=if(Y=2) then Y + 1 else 3}

# Function plumbing.
test(S00){F(a:any)(f()<decides>:type{3}):=123}
test(S00){F(a:any)(f()<decides>:type{a=3}):=123}
test(D00){F(a:any)(f()<decides>:type{a=3}):=f[]}
test(S00){F(a:any)(f()<decides>:type{a=3})<resolves>:={x:=f[]}}
test(D00){F(a:any)(f()<decides>:type{a=3})<resolves>:={x:=f[]; expect<succeeds>{x=3}}}
test(S00){F(a:any)(f()<decides>()<succeeds>:int):=123}
test(S00){F(a:any)(f()<decides>():type{g():int}):=123}
test(S00){F(a:any)(f()<decides>:type{a=3})<resolves>:={x:=f[]}}
test(D00){F(a:any)(f()<decides>:type{a=3})<resolves>:={x:=f[]; expect<succeeds>{x=3}}}

#
# Advanced: Functions with simple imperative effects.
#

# Implication effects.
test(S00){f(g():int):int => h():int => f[h]}
test(U00){f(g():int):int => h()<interacts>:int => f[h]}
test(S00){f(g():int):int => h()<interacts>:int => assume{f[h]}}
#test(D00){f(g():int):int => h()<interacts>:int => assume<decides>{f[h]}} # What should this do?

# Call effects allowability.
test(X00){if{Print(123)}}
test(X00){for{Print(123)}}
test(S00){f(123)<interacts>:=Print(456)}
test(X00){Print[123] => 456}
test(X00){Print(123) => 456}
test(X00){if{Print[123]}}
test(X00){for{Print[123]}}
test(S00){g(f():=3)<interacts>:=Print(3)}
#test(S00){f()<interacts>:=Print(456); f[]}

# Stuff like this is the caller's problem, because we can't hope to reason
# here about references to open world idempotent overloads.
test(S00){f():Print => 1}
test(S00){f():Print => f[]}
test(S00){f():Print => if{f[]}}
test(X00){f(x:Print):=2}

# Broke with ArrayMacro input improvements, now fixed.
test(D00){a&b:int => f(x&y:int):=x=y => g(a):int => h(b):int => f[g[1],h[2]]}
test(F00){a&b&c:int => f(a,x&y:int):type{x=y} => g(b):int => h(c):int => f[3,g[1],h[2]]}

# Throw.
test(X00){if{Throw[123]}}
test(X00){if{expect<succeeds>{Throw[123]}}}
test(X00){f():=Throw(3)}
#test(S00){f()<throws_any>:=Throw(3)}
#test(S00){f()<throws_any>:=Throw(4)}
#test(S00){f(x:int)<throws_any>:=Throw(x)}
#test(S00){f()<throws_any>:={2=3,Throw(4)}}
#test(S00){f()<throws_any>:={Throw(4),2=3}}

#
# Advanced: Functions of all effects.
#

# Nested functions with multiplicity effects.
test(S00){f()<decides>:int => f[] => 1}
test(S00){f(x:int)<decides>(y:int):int => 123}
test(S00){f(x:int)<decides>(y:int):int => f[3]}
test(S00){f(x:int)(y:int)<decides>:int => 123}
test(S00){f(x:int)(y:int)<decides>:=2  => f[3]}
test(S00){f(x:int)(y:int)<decides>:int => f[3]}
test(D00){f(x:int)(y:int)<decides>:int => f[3][4]}
test(S00){f(x:int)<decides>(y:int)<decides>:int => 123}
test(S00){f(x:int)<decides>(y:int)<decides>:int => f[3]}
test(D00){f(x:int)<decides>(y:int)<decides>:int => f[3][4]}
test(D00){f(x:int)<decides>:type{g(y:int):int} => f[3]}
test(S00){f(x:int)<reads>:=x}

# Functions on spine must meet their specification.
test(S00){f(a:int):=g()<decides>:=a=3}
test(D00){f(a:int):=g()<decides>:=h():=a=3}

# Abstraction strength study.
test(D00){x:int => f():=x=3 => 9}
test(S00){x:int => f()<decides>:=x=3 => 9}
test(S00){x:int => (f():=x=3,:type{f[]}) => 9} # This is ok. f() has a head, and independently f=fails.

# Trying unsuccessfully to to escalate <decides> functions into contradictions.
test(S00){g(x:int)<decides>:=nat[x]; f(y:g):=g[y]}
test(D00){f(x:int)<decides>:=let(nat[x]) do :int=4; a:int => f[a]}
test(D00){f(x:int)<decides>:=let(nat[x]) do :int => f[3]}
test(D00){f(x:int)<decides>:=let(nat[x]) do :type{x=3} => f[3]}
test(S00){a:int => f(x:int):=let(int[x]) do :type{x=3} => f[a]}
test(S00){a:int => f(x:int):=let(nat[x]) do :type{x=3} => f[a]}
test(D00){a:int => f(x:int):=let(nat[x]) do :int => f[a]}
test(D00){a:int => f(x:int)<decides>:=let(nat[x]) do :int<succeeds>=4 => f[a]}
test(U00){a:int => b:any => f(x:int)<resolves>:=let(x=b) do :int<succeeds>=4 => f[a]}

# Eta conversion with inflexible calls.
test(U00){p&q:int => as:=array{a:any} => f(p):=as[0]=3 => f[q]}
test(D00){p&q:int => as:=array{a:any} => f(p):type{as[0]=3} => f[q]}
test(U00){a:int => b:any => f(x:int):=int[x] => if{a=b,f[b],int[b]}}
test(U00){a:int => f(x&y:any):=x=y => {f[a,a],a=2}}
test(D00){a:int => f(x&y:any):type{x=y} => {f[a,a],a=2}}
test(D00){c:int => f(c):type{a=b where a:any,b:any} => f[3]}
test(U00){a:int => f(x:nat):any => f[a]=f[a]}
test(D00){a:int => f(x:nat)<computes>:any => f[a]=f[a]}
test(U00){a&b:int => f(x:nat):any => eq(x&y:any):=x=y => {x:=f[a],y:=f[b],eq[x,y]}}
test(D00){a&b:int => f(x:nat):any => eq(x&y:any):type{x=y} => {x:=f[a],y:=f[b],eq[x,y]}}
test(U00){a&b:int => f(:int):any => m(:nat):type{a=b} => {f[a]=f[b],m[a]}}

# These were <decides> before we weakened <decides> flow. 
# We have decides{f[a]=f[3]}, But the assumed f-result :any flows in with <resolves>.
# This is exactly as our new rule intended, so we can conclude in a:any => x:=a=3 that decides{x=3} and resolves{x=a}.
# REALIZATION: U00 IS THE ONLY SOUND ANSWER ENSURING THE RUNTIME CAN'T GO WRONG! Runtime has no (eq-call-computes) logic!
test(U00){a:int => f(:int)<computes>:any => a=3 where f[a]=f[3]}
test(U00){a&b:int => f(:int)<computes>:any => m(:nat):type{a=b} => {f[a]=f[b]; m[a]}}
test(U00){a&b:int => f(:int)<computes>:any => m(:nat):=a=b      => {f[a]=f[b]; m[a]}}

# Lambdas containing weak constructs in their domains. Not sure whether these should be accepted.
#test(V00){check<decides >{1=2} => 456}
#test(V00){check<fails   >{1=2} => 456}

# Functions with <fails>.
test(S01){g0(:int)<fails>:=3}
test(S00){g0(:int)<fails>:false}
test(S00){g0(:false):=3}
test(S00){f()<fails>:false => 123}
test(S00){f(g0(:int)<fails>:false):=4 => f(false)}
test(S00){f(g0(:int)<fails>:false):=4 => f(g1(x:false):=3)}
test(S01){f(g0(:int)<fails>:=3):=4 => 5}
#test(P00){g(f(:int)<fails>:false):=3; g(int)}
test(S00){g(f(:int)<fails>:false):=3; g(false)}
test(S00){g(f(:int)<fails>:false):=3; g(h(:int)<fails>:false)}
test(A00){g(f(:int)<fails>:false):=3; g(h(:int)<fails>:int)}
test(A00){g(f(:int)<fails>:false):=3; g(h(:int)<decides>:int)}
#test(U00){g(f(:int)<fails>:false):=3; g(h(:int):=4)}
#test(P00){g(f(:int)<fails>:false):=3; g(h(:int):=4)}
#test(P00){f(g0(:int)<fails>:false):=4 => f(g1(x:int):=3)}

# Simple function <fails>.
test(S00){function(1)<fails>{1=2}}
test(S00){function(1)<fails>{:false}}
test(A00){function(1)<fails>{:int}}
test(S01){function(1)<fails>{1}}
test(S01){function(x:int)<fails>{x}}
test(S00){function(x:int)<fails>{x='x'}}

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
test(D00){f()<decides>:int=3; f[]}
test(D00){f()<decides>:int => f[]}
test(U00){f():any => f[]=7}
test(U00){f()<decides>:any => f[]=7}
test(D00){f()<decides>:int => f[]}

# Effects specifiers on type abstractions.
test(D00){f()<decides>:int<resolves> => f[]}
test(D00){f()<decides>:int<decides>  => f[]}
test(S00){f()<decides>:int<succeeds> => f[]}

# Effects abstraction checks.
test(S00){f(x:int)<succeeds>:int<succeeds>=3}
test(V10){f(x:int)<succeeds>:int<succeeds>=x=3}
test(V10){f(x:int)<succeeds>:int<succeeds>:false}
test(S00){f(x:int)<decides >:int<succeeds>=3}
test(V10){f(x:int)<decides >:int<succeeds>=x=3}
test(V10){f(x:int)<decides >:int<succeeds>:false}
test(S00){f(x:int)<decides >:int<decides>=3}
test(S00){f(x:int)<decides >:int=x=3}
test(S00){f(x:int)<decides >:int:false}
test(S00){f(x:int)<decides >:int<resolves>=3} # Because declared fx act as a mask.
test(S00){f(x:int)<resolves>:int<succeeds>=3}
test(S00){f(x:int)<resolves>:int<decides>=3}
test(S00){f(x:int)<resolves>:int<resolves>=3}
test(S00){f(x:int)<decides >:=x=2}
test(D00){f(x:int)<fails   >:=x=2}
test(S01){f(x:int)<fails   >:=2}

# <diverges>.
test(S01){f()<diverges>:=1}
test(F00){f()<diverges>:false}
test(A00){f()<diverges>:int}
#test(S00){f(x:int)<diverges>:false:=f(x)}
#test(A00){f()<diverges>:int => 1}
#test(S00){f()<diverges>:int => f[]}
#test(S00){f()<diverges>:int => f[]=2}
#test(S00){f()<diverges>:int => f()=2}
#test(S00){f()<diverges>:int => not f[]}

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
test(D00){f(g(:int)<decides>:int)<decides>:=g[3] => g(x:int)<decides>:int => f[g]} # Was D00 before we weakened <decides>.
test(S00){f(g(:int)<decides>:int)<decides>:=g[3] => f[g(x:int):int=4]}
test(D00){f(g(:int)<decides>:int)<decides>:=g[3] => f[g(x:int)<decides>:int=4]} # Was D00 before we weakened <decides>.
test(P00){f(g(:int)<decides>:int)<decides>:=g[3] => f(g(x:int)<decides>:int=4)}

# Equalty inference and effects abstraction.
test(U00){f(x:any)            :any => f(3)=f(3)}
test(U00){f(x:any)<converges >:any => f(3)=f(3)}
test(S00){f(x:any)<computes  >:any => f(3)=f(3)}
test(U00){f(x:any)<reads     >:any => f(3)=f(3)}
test(U00){f(x:any)<writes    >:any => f(3)=f(3)}
test(U00){f(x:any)<transacts >:any => f(3)=f(3)}
#test(S00){f(x:int)<computes><diverges>:any => if{f[3]=f[3]}}
test(S00){f(x:int)<computes><succeeds >:any => if{f[3]=f[3]}}
test(U00){g(f(x:any)<throws_any>:any)<throws_any>:=f(3)=f(3)}
test(U00){g(f(x:any)<computes><throws_any>:any)<throws_any>:=f(3)=f(3)}
test(U00){g(f(x:any)<interacts           >:any)<interacts>:=f(3)=f(3)}
#test(S00){g(f(x:int)<computes><fails     >:any) := if{f[3]=f[3]}}
test(S00){g(f(x:int)<computes><decides   >:any) := if{f[3]=f[3]}}
test(U00){g(f(x:int)<computes><resolves  >:any) := if{f[3]=f[3]}}
#test(X31){g(f(x:int)<computes><iterates  >:any) := if{f[3]=f[3]}} # error dependency analysis would fix error choice
#test(X31){g(f(x:int)<computes><abstracts >:any) := if{f[3]=f[3]}}

# Abstract functions: effects declaration says what effects the assumed range may have.
test(S00){f(x:int)           :int => f[3]}
test(D00){f(x:int)<decides  >:int => f[3]}
test(U00){f(x:int)<resolves >:int => f[3]}
test(P00){f(x:int)<decides  >:int => f(3)}
test(P00){f(x:int)<resolves >:int => f(3)}
test(S00){f(x:int)<interacts>:int => 3}
test(S00){f(x:int)<interacts>:int=3; f[3]}
test(S00){f(x:int)<interacts>:int=3; f(3)}
#test(X00){f(x:int)<interacts>:int=3; if{f(3)}} # just a bad error

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
test(X31){f()<iterates>:=1}
test(X31){f()<abstracts>:=1}

# Function calling with multiplicity.
test(S00){f(x:int)<succeeds>:=x,f(3)}
#
test(S00){f(x:int)<decides>:=x,f(3)}
test(F00){f(x:int)<decides>:false,f[3]}
test(P00){f(x:int)<decides>:false,f(3)}
test(S00){f(x:int)<decides>:=x=3}
test(S00){f(x:int)<decides>:=x=3,f[3]}
test(S00){f(x:int)<decides>:=x=3,f(3)}
test(F00){f(x:int)<decides>:=x=3,f[4]}
test(P00){f(x:int)<decides>:=x=3,f(4)}

test(S00){f(x:int)<decides>:=x=3, f[:int]}
test(F00){f(x:int)<decides>:=x=3, f[:char]}
test(D00){f(x:int)<decides>:=x=3, y:int=>f[y]}
test(S00){f(x:int)<decides>:=x=3 => f[:int]}
test(F00){f(x:int)<decides>:=x=3 => f[:char]}
test(D00){f(x:int)<decides>:=x=3 => y:int=>f[y]}

# Why no P00 in hold code path with P00 recursing?
test(P00){f(g(:int)<computes>:int):int => f(g(x:int):type{3}=3)}
test(S00){f(g(:int)<computes>:int):int => f(g(x:int)<computes>:type{3}=3)}

#
# Advanced: Higher order functions of all effects.
#

# Assuming range:
test(S00){f(x:int)<decides>:=x=3, a:int => f[a] => a=3} # Range assumes as strong as caller.
test(D00){f(x:int)<decides>:=x=3 => a:int => f[a]}      # Range doesn't assume otherwise.
test(S00){f(x:int)<decides>:=x=3 => a:=3  => f[a]}

# Shows need for the tighter new FunctionMacro non-succeeds rules.
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:any     ):=3]}
test(S00){f(g0(:int)<decides>:int):=3 => f[g1(x:rational):=3]}
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
test(S00){a:int => g(f()(x:int):=3):=4 => assume<succeeds>{g[f1()(x:int):=a]}}
#test(D00){a:int => g(f()(x:int):=3):=4 => assume<decides >{g[f1()(x:int):=a]}} # What should this do?
#test(D00){a:any => g(f()(x:int):=3):=4 => assume<decides >{g[f1()(x:int):=a]}}

# Function effects inclusion.
test(U00){f(g()<computes>:int):int => g()<interacts>:int => f[g]}
test(U00){f(g()<computes>:int):int => g():int => f[g]}
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
#test(S00){f(g()<decides >:int):int => g()<decides  >:int => f(g)} # Broke when we weakened <decides>.
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

# Requires abstract treatment of natives and effects.
test(S00){f(g()<interacts>:any):=3}
test(S00){f(g(:any)<interacts>:any):=4 => f[Print]}
test(S00){h(f(g(:any)<interacts>:any)<interacts>:=g[4])<interacts>:=f[Print]}
test(S00){h(f(g(:any)<interacts>:any)<interacts>:=g[4])<interacts>:=f(Print)}
test(U00){h(f(g(:any):any)<interacts>:=g[4])<interacts>:=f[Print]}
test(P00){h(f(g(:any):any)<interacts>:=g[4])<interacts>:=f(Print)}
test(X00){h(f(g(:any)<interacts>:any):=g[4])<interacts>:=f[Print]}

#
# Advanced: Function Abstraction.
#

#
# Advanced: Functions with <open> and <closed>.
#

#
# Advanced: Functions and circularity sans stage.
#

#
# Advanced: Scalar Data Flow Modulo Assumptions.
#

#
# Advanced: Equality inference with <computes>.
#

#
# Advanced: Higher Order Unification.
#

#
# Advanced: Stage.
#

# Simple stage.
test(S00){:int=2}
test(V10){:int='c'}
test(S00){x:int=2}
test(V10){x:int='c'}
test(V10){x:int=(2=3)}
test(V10){a:int => x:int=(a=2)}
test(N00){x:int=y}
test(S00){x:int=3}
test(S00){x:int=3; int[x]}
test(D00){x:int=3; x=3}
test(D00){x:int=3; x=2}
test(D00){x:int=3; nat[x]}
test(S00){x:nat=3; int[x]}
test(F00){x:int=3; x='c'}
test(F00){x:int=3; x=false}

# Simple circular stage.
test(V10){x:int=x}
test(S00){x:int=3; y:int=4}
test(S00){x:int=3; int[x]}
test(S00){x:int=3; y:int=x}
test(S00){x:int=y; y:int=3}
test(V10){x:int=y; y:int=x}
test(S00){x:int=y; y:int=3; int[x]; int[y]}

# Stage in functions ranges and assumptions.
test(S00){f():int=3}
test(S00){f(x:int):int=3; f[4]}
test(S00){f(x:int):int=3; int[f[4]]}
test(D00){f(x:int):int=3; f[4]=5}
test(F00){f(x:int):int=3; f[4]='x'}
test(S00){f(x:int):int=x; f[4]}
test(D00){f(x:int):int=x; f[4]=4}
test(S00){x:int => y:int=x}
test(S00){assume{x:int=3}}
test(R00){assume{x:int=(:int)}}
test(R00){assume{x:int:int}}
test(S00){g():int=3 => g[]}
test(S00){g():int=3 => g()}
test(D00){g():int=3 => g[]=3}
test(D00){g():int=3 => g()=3}
test(D00){g():int=3 => g[]=4}
test(D00){g():int=3 => g()=4}
test(F00){g():int=3 => g[]='c'}
test(F00){g():int=3 => g()='c'}
#test(S00){f(x:int):int=x; assert{f[4]=4}}
#test(R01){f(x:int):int=x; assert{f[4]=5}}

# Stage soundness preventing circularity.
test(S00){a:int => {f():type{int[a]}=a; f[]}}
test(V10){a:int => {f():type{a=3}=a=3; f[]}}
test(V10){a:int => f():type{a=3}=a=3}

# Stage soundness required being contingent on domain.
test(S00){a&b:int => x:=a=b => :type{a=b}=x}
test(D00){a&b:int => f(a=b):type{a=b} => f[_]}
test(S00){a&b:int => f(x:=a=b):type{a=b}=x}

# Staging with recursive references.
test(S00){y:=(3,y[0])}
test(S00){x:any=y:=7}
test(S00){x:any=y:=(3,y[0])}

# Stage recursion and arrays.
test(S00){xs:type{3,:int}=(3,5)}
test(S00){xs:type{3,:int}=(3,5); xs}
test(S00){xs:type{3,:int}=(3,5); xs[0]}
test(S00){xs:type{3,:int}=(3,5); xs[0]=3}
test(F00){xs:type{3,:int}=(3,5); xs[0]=4}
test(S00){xs:type{3,:int}=(3,5); xs[0]=3}
test(F00){xs:type{3,:int}=(3,5); xs[0]=4}
test(V10){a:any=(1,a)}
test(V10){a:any=f():=a}

# Recursive reference out of stage.
test(S00){x:int=3 where 4}
test(V10){x:int=3 where x}
test(V10){x:int=3 where int[x]}
test(S00){x:type{3}=x}
test(V10){x:int=x}
#test(S00){x:int=let(x) do 3}
#test(S00){xs:type{3,:int}=let(xs) do (3,5)}
#test(S00){xs:type{3,:int}=(xs[0],7)}
#test(S00){xs:type{3,:int}=(xs[0],xs[0])}

# Stage recursion. We could tighten this in the future with a different structure.
test(S00){xs:type{:int,:int}=ys:=(3,ys[0])}
test(S00){xs:type{:int,:int}=ys:=(ys[1],3)}
test(V10){xs:type{:int,:int}=(3,xs[0])}
test(V10){xs:type{:int,:int}=(xs[1],3)}
test(V10){xs:type{array{:int}}=array{xs[0]}}
test(V10){xs:type{:int,:int}=(xs[0],3)}
test(V10){xs:type{:int,:int}=(3,xs[1])}

# Staging with function abstraction.
test(S00){f(x:prefix'-'):=x}
test(S00){f(x:prefix'-'):=x,f[7]=-7}
test(F00){f(x:prefix'-'):=x,f[7]=7}
test(S00){x:prefix'-'=3}
test(U00){x:prefix'-'=3,x=0}
test(U00){x:prefix'-'=3,x=3}
test(U00){x:prefix'-'=0,x=0}
test(U00){x:prefix'-'=3,x=-3}
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

# Stage containing conditions and resolves-detection across contexts.
test(V10){a:any=if(a){3}}
test(V10){a:any=if(b:=a){b}}

# Stage functions and running.
test(S00){f():int=3}
test(V10){f():nat=-3}
test(S00){f():int=0,int[f[]]}
test(D00){f():int=0,nat[f[]]}
test(S00){f(x:=0|1):nat=x}
test(S00){f(1):=2}
test(S00){f(x:int):int=x}
test(V10){f(x:int):char=x}

# Stage and higher order functions.
test(S00){f(g(:int):int):int => f[g(x:int):int=x]}
test(S00){f(g(:int):int):int => f(g(x:int):int=x)}

#
# Advanced: Stage + exploration.
#

# Stage containing conditions, trying to cheat.
test(S00){a:int => int[if(a=3) then 3 else 4]}
test(S00){a:int => :int=if(8=9) then 3 else 4}
test(S00){a:int => :int=if(8=9) then 3 else 4}
test(V10){x:int=if(x=3) then 3 else 4} # Could accept this.
test(V10){x:int=if(x=3) then 4 else 5}
test(V10){x:int=y,if(x=1) {y=1} else {y=2},y:int}
test(S00){a:int => :int=if(a=3) then 3 else 4}

# Stage and exploration.
test(S00){f(a:int):nat=if(a=1){2}else{3}}
test(V10){f(a:int):nat=if(a=1){2}else{-3}}
test(V10){f(a:int):nat=if(a=1){-2}else{3}}
test(V10){f(a:int):nat=if(a=1){-2}else{-3}}
test(S00){a:int=1; :nat=if(a=1){2}else{3}}
test(V10){a:int=1; :nat=if(a=1){2}else{-3}}
test(V10){a:int=1; :nat=if(a=1){-2}else{3}}
test(V10){a:int=1; :nat=if(a=1){-2}else{-3}}
#a:int=3; x:nat=if(a=1){2}else{-2}

# Conditions directly containing stage.
test(S00){if{x:int<succeeds>=3}}
test(S00){if{x:int<decides>=3}}
test(S00){if{x:int<decides>:false}}
test(S00){a:any => if{x:int<decides>=3}}
test(S00){a:any => if{x:int<decides>:false}}
test(V10){a:any => if{x:int<decides>:=a}}
test(S00){a:int => if{x:int<decides>=a=3}}
test(D00){f(a:int):=if(:int<decides>=a=3) then a=3}
test(A00){if{x:int=3}} # Because <iterates> and we don't have a generalization fallback yet.
test(A00){f(a:int):=if(:int=a=3) then a=3}

# Conditions reaching stage through beta.
test(S00){f()<decides>:any => if{f[]}}
test(S00){f()<decides>:int => if{f[]}}
test(U00){f()<resolves>:int => if{f[]}}
test(S00){f():any=3; if{f[]}}
test(S00){f():int=3; if{f[]}}
#test(S00){f()<decides>:any=3; if{f[]}}
#test(S00){f()<decides>:int=3; if{f[]}}
test(U00){f()<resolves>:int=3; if{f[]}}
test(X00){f(x:int)<interacts>:int=3; if{f[2]}}
test(X00){f(x:int)<interacts>:int=3; if{f(2)}}

# Stage and negotiability boundaries around iterate.
test(S00){a:any => {x:int=x; assume{x=3}}}
test(V10){a:any => {x:int=x; if{int[x]} then assume{x=3}}}
test(V10){a:any => {x:int=a; if{int[x]} then assume{x=3}}}
test(A00){a:any => f(:int)<decides>:int => if{x:=f[x]}}
test(U00){a:any => f(:int)<decides>:int => if{x:=f[a]}}

# Stage-fails.
#test(S00){if{x:int<fails>:=false?}}
#test(S00){if{x:int<fails>:=3}}

# Stage nesting.
test(S00){x:int:int = 3}
test(S00){assume{f():int:nat}}
test(S00){assume{f():nat:nat}}
test(S00){f():int:nat => 123}
test(S00){f():int:nat => int[f[]]}
test(D00){f():int:nat => nat[f[]]}
test(V10){f():nat:int => nat[f[]]}

# Stage in assumed functions and nested function domains.
test(S00){assume{f():int=(:int=3)}}
#NEW_IN test(S00){assume{f()<decides>:int=(:int)}}
#NEW_IN test(S00){assume{f():int=(:int)}}
test(S00){assume{f():int=let(a:nat=3) do a}}
test(S00){assume{f():int=3}}
test(S00){f():int=3 => int[f[]]}

# Mixing abstractions and tests.
test(R00){assume{a:int; int[a]}}
test(S00){assume{f():={int[a]; a:int}}}
test(U00){assume{f():={a:int; int[a]}}}
#test(S00){assume{f():={a:int; b:int=a}}}

# Modulate staged expression multiplicity by assumed multiplicity.
test(S00){assume{x:int<decides>=3}}
test(S00){f(a:int)<decides>:=assume{x:int<decides>=a}}
test(V10){a:int => if{x:int<succeeds>=a=3}}
test(V10){a:int => if{x:int<fails>=a=3}}
test(S00){f()<decides>:int => f[] => 123}
test(S00){f()<decides>:int => assume{f[]}}
test(S00){f()<decides>:int => if{f[]}}
test(U00){f()<resolves>:int => if{f[]}}
test(S00){f(a:int):=if(a=3) then a=3}
#test(V10){f(a:int)<decides>:=assume{x:int<decides>=a=3}} # Figure out how these specifiers should compose; S00 might be right here.

# Assumption forks inside stage.
test(S00){f():type{3} => f[]=3}
test(S00){f():type{3|3} => f[]=3}
test(S00){f():type{int|int} => f[][3]}
test(S00){f():type{int|int} => :f[]=3}
test(S00){f():type{int|nat} => {x:f[]=3}}
test(S00){f():type{int|int} => {x:f[]=-3}}
test(V10){f():type{int|nat} => {x:f[]=-3}}
test(V10){f():type{nat|int} => {x:f[]=-3}}
test(V10){f(x:=-1|1):nat=x}
test(V10){f(x:=1|-1):nat=x}
test(S00){f(x:=0|1):int=x}
test(V10){f(x:=-1..1):nat=x}
#test(S00){f(x:=0..1):nat=x; for(x:=f[_]) do nat[x]} # Functions must intersection-iterate over domain matches.

# Stage + if.
test(S00){x:int=if(1) then 1 else 2}
test(S00){x:int=if(1=2) then 1 else 2}
test(S00){a:int => :int=3}
test(S00){a:int => :int=a}
test(S00){a:int => :int=if(1) then 2 else 3}
test(S00){a:int => :int=if(1=2) then 2 else 3}
test(S00){a:int => if(a=1) then :int=2 else :int=3}
test(S00){a:int => :int=if(a=1) then 2 else 3}

# Sonnenberg found problems with stage.
test(S00){x:type{3}=x}
test(V10){x:type{3|4}=x}
test(S00){if{x:type{3|3}=x}}
test(S00){if{x:type{3|4}=x}}
test(S00){a:=if{x:type{3|3}=x}}
test(S00){a:=if{x:type{3|4}=x}}
#test(S00){a:=if(x:type{3|3}=x){x}; int[a]}
test(S00){for{x:type{3|3}=x}}
test(S00){for{x:type{3|4}=x}}
test(S00){Length(for{3})}
test(S00){Length(for{x:type{3}=x})}
#test(S00){Length[for{x:type{3}=x}]=1}
test(S00){Print of for{x:type{3}=x}}
test(S00){Print of for{x:type{3|3}=x}}
test(S00){Print of for{x:type{3|4}=x}}
#test(S00){for{x:type{3|4}=x}=(_,_)}

#
# Advanced: Stage + recursion.
#

# Stage and converges functions.
test(V10){f()<converges>:int=f[]}
test(P00){f()<converges>:int=f()} # Would like a better error.
test(V10){f(x:int)<converges>:int=if(x=0) then 1 else f[0]}
#test(S00){f(x:int)<converges>:=if(x=0) then :int=1 else :int=f(0)}

# Stages and non-converges functions.
#test(S00){f():int=f[]}
#test(S00){f():int=f[]; f[]} # Need error code to say "don't run".
#test(S00){f():int=if(x=0) then 1 else f()}
#test(S00){a:int => f():type{a=3}=f[]}
#test(S00){a:int => f():type{a=3}=a=3 where f[]}
#test(S00){f():int=f()}

# Stage evaluation equality inference.		
test(S00){f(x:int)<computes>:int => f[3]=f[3]}
test(S00){f(x:int)<computes>:int=4; f[3]=f[3]}
test(S00){f(x:int)<computes>:=(:int,4) => f[3]=f[3]}
test(S00){f(x:int)<computes>:=(:int,4) => f[3][0]=f[3][0]}
test(S00){f(x:int)<computes>:=(:int=3,4); f[3][0]=f[3][0]}

# Stage paraconsistency.
test(V10){f(x:int):false=3}
test(S00){f(x:false):int=x}
test(S00){f(x:false):false=x}
#test(S00){f(x:int):false=f[x]}
#test(V10){f(x:int)<converges>:false=f[x]}

# Stage in abstract contexts with inputs.
# Rejecting this is not wrong, because we're checking 3 against inflexible g-input.
# Stage right-hand expression does need that input in case it's of the form :t.
test(S00){type{x:int=3}}
test(S00){g():int=3 => 4}
test(P00){g():int=3 => g(h():=3)}
test(S00){g():type{3}=3 => 4}
test(S00){g():type{3}=3 => (h():=3)}
test(S00){g()<computes><converges>:type{3}=3 => (h():=3)}
test(S00){g()<computes>:type{3}=3 => (h():=3)}
test(S00){a&b:int => f(x:=a=b):type{a=b}=x => 3}
test(V10){a:int => {f():type{a=3}=a=3; f[]}}
test(U00){a:any => f(x:int):int=x => f[a]}
test(D00){a:int => f(x:=a=3):type{a=3}=x => f[_]} # Shows stage beta must weaken by ImpliedFx.

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

# Cyclic functions with stage.
test(S00){f():any=f}
test(S00){f(f):any=3}
test(S00){f(f):any=f}

#
# Advanced: Resolution consistency across control flow forks.
#

#
# Advanced: Inference weakening to head normal forms; WhenCastStep.
#

# WhenCastStep.
test(S00){f(x:any):type{x=3} => f[_]}
test(A00){prefix'-'[_]}
test(A00){prefix'-'[]} # Could be U00 but missing axiom.
test(M00){1{2}}
test(M01){_{2}}

# Imply-succeeds WhenCastStep-non-succeeds must not escalate FailTargetStep to contradiction.
test(F00){f():nat => f()=-7}
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
test(F00){F(x:int)<resolves>:=expect<succeeds>{(3,5)[x]} where x=9}
test(S00){x:=nat['c'] => x='d'}
test(S00){x:=nat[-3] => x='c'}
test(F00){x:int => x=false}
test(F00){x:int=3,x='c'}
test(F00){x:int=3,x=false}
test(F00){f(x:int):int=3; f[4]='x'}

# We need flow-independent WhenCastStep heads to make assumptions effective.
test(S00){x:int => x=3 => x+1=4}
test(F00){x:int => x=3 => x+1=5}

# Propagation of decidability through head reification via WhenCastStep.
test(S00){x:=2,x+3=5}
test(F00){x:=2,x+3=6}
test(S00){F(x:int)(x=2):=x+3}
test(S00){F(x:int)(x=2):=x+3=5}
test(F00){F(x:int)(x=2):=x+3=6}
test(D00){F(x:int):=x=2 where x+3=5}             # Obvious.
test(S00){F(x:any):=assume{x=2} where x+3=5}
test(F00){F(x:any):=assume{x=2} where x+3=6}
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x=2}}        # Obvious.
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3=5}}      # Must hold, else it implies x=2.
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3}}        # Must hold too, as it's no weaker than previous.
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{int[x]=2}}   # Must hold, else it implies x=2.
test(S00){F(x:int)<resolves>:=x=2 where expect<succeeds>{int[x]}}     # Can succeed by reachability.
test(S00){F(x:int)<resolves>:=x=2 where expect<succeeds>{int[x]=x}}   # Can succeed by reachability.
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3=5}}
test(F00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3=6}}
test(U00){F(x:any)<resolves>:=x=2 where expect<succeeds>{x+3=5}}
test(D00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3=5}}
test(F00){F(x:int)<resolves>:=x=2 where expect<succeeds>{x+3=6}}
test(F00){F(x:int)<resolves>:=x=1 where expect<succeeds>{(5,7)[x]=5}}
test(D00){F(x:int)<resolves>:=x=1 where expect<succeeds>{(5,7)[x]}}
test(D00){F(x:int)<resolves>:=x=1 where expect<succeeds>{(5,7)[x]=7}}
test(F00){F(x:any)<resolves>:=x=(3,4) where expect<succeeds>{Concatenate[x,(5,6)]=(3,4,5,7)}}
test(U00){F(x:int):=(5,7)[x]=7} # Can narrow to D00.
test(S00){F(x:any):=assume{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,6)}
test(F00){F(x:any):=assume{x=(3,4)} where Concatenate[x,(5,6)]=(3,4,5,7)}

#
# Advanced: Paraconsistency.
# When making inferences in a context that <fails> or <contradicts>, we want to disable verifier
# error checking on operations entailed by a contradiction (as determined by data flow and equations),
# while continuing normal error checking on everything else.
#

# The empty array. Subtyping requires this succeed!
#test(S00){x:false => false[x]}
#test(S00){x:false => false(x)}
#test(S00){f(x:false,y:int):=false[x]}

# "Contradiction is contained at projections" was the former title of this section.
test(S00){as:=(7,8=9) => as=2}
test(S00){as:=(7,8=9) => as=(_,_)}
test(S00){as:=(7,8=9) => as[0]}
test(F00){as:=(7,8=9) => as[0]=6}
test(S00){as:=(7,8=9) => as[0]=7}
test(S00){as:=(7,8=9) => as[0]=8} # questionable due to constant degeneracy
test(S00){as:=(7,8=9) => as[0]=9} # questionable due to constant degeneracy
test(F00){as:=(7,8=9) => as[0]=10}
test(S00){as:=(7,8=9) => as[1]=2 where 2=3}
test(F00){as:=(7,8=9) => as[0]=2}
test(F00){as:=(7,8=9) => as where 2=3}
test(F00){as:=(7,8=9) => as[0] where 2=3}
test(F00){as:=(2,3=4) => as where 8=9}
test(F00){as:=(7,8=9) => as=(_,_) where 2=3}
test(F00){as:=(7,8=9) => as[1] where 2=3}

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
test(S00){F()<resolves>:={x:=assume{:false}}}
test(S00){F()<resolves>:={x:=assume{:false},prefix'-'[x]}}
test(S00){F()<resolves>:={x:false}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{prefix'-'[x]}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{-x}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{-x=7}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{x+1}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{1+x}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{x+1=7}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{1+x=7}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{operator'+'[x]}}}
test(F00){F()<resolves>:={x:false,expect<succeeds>{operator'+'[x]=7}}}

# Calling failing and contradictory functions.
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

# Uninhabited parameters to types.
test(S00){x:false => any[x]}
test(S00){x:false => int[x]}
test(S00){x:false => comparable[x]}
test(S00){x:false => int[x]}
test(S00){x:false => int(x)}
test(S00){f():false => f[]}
test(S00){f():false => f()}

# Uninhabited container parameter.
test(S00){x:false => (1,2)[x]}
test(S00){x:false => (1,2)[x]=3}
test(F00){F()<resolves>:=expect<succeeds>{(1,2)[x]}   where x:false}
test(F00){F()<resolves>:=expect<succeeds>{(1,2)[x]=3} where x:false}

# Uninhabited dot expressions.
test(N05){int.sym}
test(S00){f(x:=1=2):=x.blah}
test(S00){f(x:false):=x.blah} 
test(N05){f(x:false,y:int):=y.blah}
test(S00){f(x:false,y:int):=x.blah}
test(S00){as:=(1,2)=3 => as.blah}
test(S00){as:=(1,2)=3 => as[1].blah} 
test(F00){x:=1=2,x.blah}
test(F00){x.blah,x:=1=2}
test(F00){F()<resolves>:={x:=1=2,expect<succeeds>{x.blah}}}
test(F00){F()<resolves>:={expect<succeeds>{x.blah},x:=1=2}}
test(F00){(1=2).blah}

# Paraconsistency.
test(U00){assume{x:any   where 3=4}=5}
test(D00){assume{x:int   where 3=4}=5}
test(R20){assume{x:=5    where 3=4}=5}
test(F00){assume{x:=6    where 3=4}=5}
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
test(F00){assume{2=3},4=5}
test(F00){assume{false[]},4=5}
test(F00){a:=2,assume{a=3},4=5}

# Don't let recurses overwhelms fails.
test(R20){x:=2,assume{x=3}}
test(S00){0 => Err("Hello")};
test(R01){Err()};
test(R01){Err("Hello")};
test(F00){Err(),false[]}
test(F00){false[],Err()}
test(F00){0 => {Err("Hello"),2=3}};
test(F00){0 => {2=3,Err("Hello")}};

# Calling contradictory arrays and functions produces contradictory result.
test(U00){a&b:any => (a,1)=2 => b=3}
test(D00){a&b:comparable => (a,1)=b => a=3}
test(S00){f():=2 => f=3 => f[]=4}
test(U00){a:any => (a,1)=2 => a=3}

# Conditions and contradiction.
test(S00){if{x:=1=2,if{x=3}}}
test(F00){f(x:false):=if(x=1){1}else{2}=3}
test(D00){a&b:int => f():false => {a=b,if{a=b} else f()}}
test(D00){a&b:int => f():false => {if{a=b} else f(),a=b}}
test(S00){a&b:int => f():false => f()}
test(S00){a&b:int => f():false => if{a=b} else f()}
test(S00){if{x:false,if{x=3}}}

# Should any multiplicity be allowed under a contradictory condition?
# Was S00, now F00, need axiomatic semantics to weigh in.
test(F00){f(x:false):=if(x){1}else{2}=3}

# Inference in assumptions must not circularly escalate the strength of weaker inferences.
test(F00){F(x:any)<resolves>:=assume<decides>{x=3} where expect<succeeds>{x=4}}
test(D00){F(x:any)<resolves>:=assume<decides>{x=3} where expect<succeeds>{x=3}}
test(D00){F(x:any)<resolves>:=assume<decides>{x=3} where assume{x+1} where expect<succeeds>{x=3}}

# S00 is correct for now, but should be disallowed when operator'+' properly overloads:
test(S00){F(x:any)<resolves>:=assume<decides>{x=3} where assume{x+1}}
test(S00){F(x:any)<resolves>:=assume<decides>{x=3} where assume{x+1} where expect<succeeds>{x+1}=4}
test(S00){F(x:any)<resolves>:=assume<decides>{x=3} where assume{x+1=4}}

# Succeeding unifications and divergence.
test(F00){:false}
test(R01){Err("yo")=1}
test(S00){f(x:=Err[]):=x=1}
test(S00){f(x:=Err()):=x=1}

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
test(F00){as:=(2,3=4) => 2=5}
test(F00){(a:=2,b:=3=4) => a=7}

# Prior problems due to constant degeneracy.
test(S00){array{7,8} where if{1=0 where array{7,9}}}
test(F00){f(x:int)<computes>:=x => f[7]=8}
test(F00){f(x:int):=x => f[7]=8}
test(D00){a:int => f(x:int)<computes>:=x => f[7]=a}
test(D00){a:int => f(x:int):=x => f[7]=a}

# Head contradiction must be independent of visibility for assumptions to be effective and confluent.
test(C00){x:any => x[0]}
test(C00){x:int => x[0]}
test(S00){x:int => assume{x=(3,4)} where x[0]}
test(S00){x:int => y:=x where assume{y=(3,4)} where x[0]}
test(S00){x:int => y:=x where assume{x=(3,4)} where y[0]}
test(S00){x:any => y:=x where assume{x=(3,4)} where y[0]}
test(S00){x:any => y:=x where assume{y=(3,4)} where y[0]}
test(S00){x:int => y:=x where assume{y=(3,4)} where y[0]}

# Tests that broke in the past due to reification atom race.
# These fork because reify stalls rather than handling paraconsistency, causing our intermediate arrays to expand.
test(D00){x:int => f():=x=7 => 3}
test(D00){x:int => f()<decides>:=x=7 => f[]}
test(S00){x:int => f()<decides>:=x=7 => if{f[]}}
test(F00){x:=x => f():=x=7 => 3=0 where f[]}
test(F00){x:=x => f():=x=7 => 3=9 where f[]}
test(S00){x:int => f():type{x=7} => {if(x=6){9}else{3}=3,f[]}}

# Contradictory variables.
test(S00){f(x:false):=x=1}
test(S00){f(x:false):=x=2}
test(S00){f(x:false):=x=1=2}
test(S00){f(x:=int['c']):=x=1}
test(F00){f(x:=int[3  ]):=x=1}
test(S00){f(x:=int[g(3):=4]):=x=1}
test(S00){f(x:=(3,5)[7]):=x=1}
#BROKE WITH INFLEXIBLE DOMAIN test(S00){f(x:=first{2=3}):=x=1}
test(S00){f(x:=1/0):=x=1}
test(F00){f(x:=(3,5)[0]):=x=1}
test(U00){f(x:false,y:any):=y=1}
test(U00){f(x:false,y:any):=y=2}
test(F00){f(x:false,y:any):=y=1=2}

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
test(F00){92=93 => 94=95}
test(F00){92=93 => 94=95,91}
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
test(F00){not 3}
#BROKE WITH INFLEXIBLE DOMAIN test(S00){x:=first{:false} => x=0}
#BROKE WITH INFLEXIBLE DOMAIN test(S00){x:=(not 3) => x=0}
#BROKE WITH INFLEXIBLE DOMAIN test(S00){x:=not 3 => x=0}

# Ranges under contradictory domains.
test(S00){1=2 => 0}
test(S00){1=2 => 1=2}
test(F00){1=2 => 3=4}
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
test(S00){a&b:int => if{x:=a=b,expect<succeeds>{x}}}
test(S00){if{x:=2=3,expect<succeeds>{x}}}
test(S00){x:=let(false[]){3} => x}
test(S00){x:=let(false[]){3} => x=3}
test(F00){x:=let(false[]){3} => x=4}
test(S00){x:=7=9 => x}
test(S00){x:=7=9 => x}
test(S00){x:=7=9 => x=7}
test(S00){x:=7=9 => x=9}
test(S00){x:=7=9 => 7=9}
test(F00){x:=7=9 => 6=8}
test(F00){x:=7=9 => 6=8}

# WhenCastStep with Paraconsistent=false provides an error.
test(M01){f(m:false):=m{}}
test(M00){if{m:=2=3; m{}}}

# Constant degeneracy influencing verifier.
test(P00){a:any => prefix'-'(a)}
test(P00){if{(a:any,2); prefix'-'(a)}}
test(P00){if{3=4; (a:any,2); prefix'-'(a)}}
test(S00){if{0=1; (a:any,2); a+1}} # Relies on constant degeneracy. 0=1 makes a:any and 2 cohabitate the same equation, so a+1 succeeds.
test(S00){array{0,1} where if{1=0 where array{0,1}}}

# Parameter degeneracy was breaking 'for'.
test(F00){for{0};1=0}
test(F00){for{2};1=0}
#test(F00){bs:=for{1},Length[bs]=0}
#test(S00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=2 or n=3}}
#test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=2}}
#test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=1 or n=2 or n=3}}
#test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=2 or n=3}}
#test(F00){as:=(:int,:int,:int) => {bs:=for(a:as,a=1){a},n:=Length[bs],n=0 or n=1 or n=3}}

# General escalation risk.
test(R20){assume{x:int   where false[]}}
test(D00){assume{x:int                }=7}
test(D00){assume{x:int   where x+2=3  }=1}
test(D00){assume{x:int   where x+2=3  }=7} # D00 because of paraconsistency.
test(D00){assume{x:int   where 8=9    }=7} # D00 because of paraconsistency.
test(D00){assume{x:int   where false[]}=7} # D00 because of paraconsistency.
test(R20){assume{x:false where false[]}=7} # Succeeds because variable contradicts.
test(R20){assume{x:false              }=7}
test(S00){a:int => assume{x:false where a=3}=7}
test(D00){a:int => assume{x:int   where a=3}=7}
test(D00){a:int => assume{x:int   where a=false}=7} # D00 because of paraconsistency.

# When we fix P00 checking problem identified elsewhere in these tests, we must rethink this question from first principles.
test(P00){F()<resolves>:={x:false,expect<succeeds>{operator'+'(x)}}}   # Should be S00.
test(P00){F()<resolves>:={x:false,expect<succeeds>{operator'+'(x)=7}}} # Should be S00.

#
# Advanced: Runtime alignment.
#

# Runtime unification - maximum asynchronous progress.
# Shows we must not resolve a flexible future to an inflexible one unless it's a head.
#test(S00){assume{x:int=x; x=3; x=3}}
#test(R20){assume{x:int=x; x=3; x=5}}
#test(R03){assume{x:int=x; not (x=3,x=5)}}
#test(S00){assume{as:type{_,_}=(_,_); as=(3,4); as=(3,4)}}
#test(R20){assume{as:type{_,_}=(_,_); as=(3,4); as=(3,5)}}
#test(S00){xs:=(3,4); ys:=(3,4); if(assume{as:type{:int,:int}=as; as=xs; as=ys}) then 1 else false?}
#test(S00){xs:=(3,4); ys:=(3,4); if(assume{x:int=3}) then 1 else 2}

# Shows that when unifying a flexible variable with an inflexible array, we need
# to make a new flexible array.
#test(R03){a:=assume{_}; for{1|(a=3; a=4)}=array{_}} # because a=3 and a=4 both get stuck
test(S00){a:=assume{_}; for{1|(b:=a,b=3,b=4)}=array{_}} # because b is flexible
test(S00){a:=assume{_,_}; for{1|(b:=a,b=(3,4),b=(3,5))}=array{_}}

# Conditions lacking runtime semantics that we need to figure out.
# Shouldn't the verifier be rejecting both conditions as abstract?
test(A00){x&y:any; x=y; if(x){x=1}}
test(A00){x&y:any; x=y; if(x=y){x=1}}
test(A00){x&y:int; x=y; if(x=y){x=1}}

# Runtime input plumbing.
test(S00){f():=1; f[]}
#test(R00){assume{f():nat; f[]}
test(S00){f():=3,f=f}
test(S00){f():=3,if{f=f}}

# Runtime localization; verifier recognizes runtime localization.
test(F00){a:int => {a=3; a=4}}
#test(F00){a:int => f():=a+1 => {a=3; f[]=5}}
#test(S00){f(a:=1|2):=(g(b:=3|4):int=a+b where f[1] where g[1]) where f[1]) where f[1]}

# Fixed runtime error: fails where disallowed.
test(S00){f(x:=(:prefix'-') where x = (:nat)):={x}; if(a:=f[3]) then Print(a) else Print("no")}
test(S00){f(x:prefix'-' where nat[x]):=x; if(f[3]) then 0 else Print[0]}

# Runtime abstracts.
test(S00){for(x:int,y:=0|1,x=y) do Print(x)}

# Old (iterate-force) inference rule was runtime-inconsistent.
test(U00){f&g:any => {x:=f=g; if(x=f){x=2=3}else{x=2=3}}}

#
# Advanced: Inputs anywhere.
#

# Investigate. Should work with no subtle considerations.
test(S00){f(a->b:prefix'-'):=(a,b) => f[3]=(3,-3)}

# Nested i->j->x::xs.
test(S00){for{x:(1,2)}}
test(S00){for{i->x:(1,2)}}
test(D01){for{i->x:=3}}
test(D01){for{i->j->x:(1,2)}}
test(S00){for(i->j->x::((6,7),(8,9))){i,j,x}=((0,0,6),(0,1,7),(1,0,8),(1,1,9))}

# Solving functions with inputs or multiple function confusion.
#test(F00){1=(2=>3)}
test(S00){H->F:type{1=>2}}
#test(...){G->F:type{1=>2}; G=(1=>2)} # Loops and not a bug: g is identity, so 1=>2 calls self.
#test(S00){G->F:type{1=>2}; H:=(1=>3); G=H}
#test(F00){G->F:type{1=>2}; H:=(1=>3); G=H}
test(S00){h->f:type{function(){_}}; g:=function(){3}; h=g}
test(S00){G->F:type{1=>2}; H:=(1=>2); G=H}
test(S00){h(1):=2 => {H->F:type{1=>2}; H=h}}
test(A00){H->F:type{1=>2}; H[1]} # Can tighten to S00 by implementing input_lambda_variable.
test(S00){f(g(1):=2):=7 => f[h(1):=2]}
test(U00){f(g(1):=2):=7 => f[h(1):=3]}
test(U00){f(g(1):=2):=7 => f[h(3):=2]}
test(S00){a->b:type{function(1){2}}; c:=function(1){2}; a=c}
test(S00){c:=function(1){2}; a=c; a->b:type{function(1){2}}}
test(A00){a->b:type{function(:int){:nat}}}
test(S00){a->b:type{function(:int){2}},c:=function(:int){2}; a=c}
test(S00){a->b:type{function(1){:int}}, c:=function(1){2}; a=c}
test(S00){a->b:type{function(:int){:int}}; c:=function(:int){2}; a=c}
test(S00){a->b:type{function(:nat){:int}}; c:=function(:int){3}; a=c}
test(S00){a->b:type{function(){:int}}; c:=function(){4}; a=c}
test(S00){a->b:type{function(:nat){3}}; c:=function(:int){3}; a=c}
test(S00){i->x:prefix'-',i=3,x=-3}
test(S00){i->x:type{1}}
test(S00){as->bs:type{(1,2)}}
test(S00){for(i->x:function(j:=3|5){j+1}){i,x}=((3,4),(5,6))}
#test(S00){(i->x,j->y):array{array{3,5}},i=0,j=1,x=3,y=5} # When we have tuple destructuring.

test(S00){a->b:type{function(2){3}}}

# Arrays with inputs.
test(S00){f(a:prefix'-',b:int):=(b,a),f[3,7]=(7,-3)}
test(F00){f(a:prefix'-',b:int):=(b,a),f[3,7]=(7,3)}

# Choice is currently diallowed at the top-level by holding iterates_pending.
# Could allow just the case of u=(v0|...|vn) where u&v&w are ground values and v0...vn are disjoint ground values,
# but this seems like an excessive special-case for functionality that so far ought not be required.
test(X00){1=(1|2)}
test(X00){2=(1|2)}
test(X00){3=(1|2)}
test(X00){f(x:int)<decides>:=x=(2|3)}
test(S00){(3,5,7)[i],i:=1}
test(F00){(3,5,7)[i],i:=4}
test(X00){1|2}
test(X00){function(1){2|3}}
test(X00){function(x:int){x=(2|3)}}
test(X00){function(x:int)<decides>{x=(2|3)}}
test(X00){f(x:int,x):=0; f[2,3|4]}
test(A00){w->x:nat}
test(S00){w->x:nat; x=3}
test(S00){w->x:nat; w=3}
test(F00){w->x:nat; x=-3}
test(F00){w->x:nat; w=-3}
test(X00){(x:int,x)=(2,2|3)}
test(X00){(x:int,x)=(2,2|1)}
test(X00){(x:int,x)=(2,1|2)}
test(X00){f(2):=0; f[3|4]}
test(X00){f(2):=0; f[2|3]}
test(X00){f(2):=0; f[1|2]}

# Basic inputs.
test(S00){a->b:type{}}
test(S00){a->b:type{2},a=2,b=2}
test(S00){c:=a->b:type{3},a=3,b=3,c=3}
test(S00){function(a->b:prefix'-'){a,b}[3]=array{3,-3}}
test(S00){for(i->x:(0,10,20)){i+x}=array{0,11,22}}

#
# Advanced: Function unification and overloading, which we completely lack right now.
#

# Function unification.
test(R04){(1=>2)=(1=>2)}

# Function eta conversion - abstract.
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{f(0)=f(0)}} # This would work if we rearranged.
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{f(0)=f(1)}}
#test(S00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{g(0)=g(0)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{g(0)=g(1)}}
#test(S00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{h(0)=h(0)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{h(0)=h(1)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{f(0)=h(0)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{f(0)=h(1)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{g(0)=h(0)}}
#test(D00){F((f&g)(:int)<computes>:int)<resolves>:=h:=f=g where expect<succeeds>{g(0)=h(1)}}
#test(S00){F((f&g)(:any)<computes>:any):=f(3)=4=>b:=g(3)=>f=g=>b=4}
#test(S00){F((f&g)(:any)<computes>:any):=f(3)=4=>b:=g(3)=>f=g=>b+1=5}

# Function eta conversion - concrete.
#test(S00){F()<resolves>:={f(:int)<computes>:=3; g(:int)<computes>:=4,h:=f=g; expect<succeeds>{f(0)=f(0)}}
#test(S00){F()<resolves>:={f(:int)<computes>:=3; g(:int)<computes>:=4,h:=f=g; expect<succeeds>{g(0)=g(0)}}
#test(R04){F()<resolves>:={f(:int)<computes>:=3; g(:int)<computes>:=4,h:=f=g; expect<succeeds>{h(0)=h(0)}}
#test(F00){F()<resolves>:={f(:int)<computes>:=3; g(:int)<computes>:=4,h:=f=g; expect<succeeds>{f(0)=h(0)}}
#test(F00){F()<resolves>:={f(:int)<computes>:=3; g(:int)<computes>:=4,h:=f=g; expect<succeeds>{g(0)=h(0)}}

#test(S00){(f&g)(x:int)<computes>:int => f=g => f[3]=g[3]}
test(U00){(f&g)(x:int)<computes>:int => if{f[3]=g[3],f=g}}

# Higher order unification of <succeeds> lambdas.
test(S00){function(){3}}
test(S00){function(){x=3; x:int}}
test(R04){function(){3}=function(){3}}
test(R04){function(){3}=function(){:int}}
test(R04){function(){:int}=function(){4}}
test(R04){function(){3,:int}=function(){:int,4}}
test(F00){function(){3}=function(){4}}
test(U00){int=prefix'-'}
test(R04){assume{int=prefix'-'}; int[0]}
#test(R04){assume{int=prefix'-'}; int[1]}
test(S00){type{f:=int=prefix'-'; expect<succeeds>{int[0]}}}
#test(S00){type{f:=int=prefix'-'; expect<succeeds>{int[1]}}

#
# Advanced: Open questions.
#

# Consider the denotational semantics of these cases. Hypothesis: It's sound.
test(A00){x:int; f(y:int)<decides>:=x=y}
test(S00){x:int; f(y:int)<decides>:=x=y; f[3]}
test(A00){f():any; f[]=3}
test(S00){g(f():any):=f[]; g(h():=3)=3}
test(S00){g(f():any):=f[]; g(h():=3)=_}
test(P00){g(f():any):=f[]; g(h():=_)=3}
test(A00){g(f():any):=f[]; g[h():=_]=3}

# Domain strength issue noted on September 2022 Japan trip.
# Hypothesis: This is sound provided that when checking an abstract :t against an input, any nested :u<succeeds>
# disallows failure anywhere in the context.
test(D00){a:int => f(i:int)<decides>:={:type{nat[i]}                 } => f[a]}
test(D00){a:int => f(i:int)<decides>:={:type{nat[i]},          nat[i]} => f[a]}
test(S00){a:int => f(i:int)<decides>:={:type{nat[i]}<succeeds>,nat[i]} => f[a]}

#
# Advanced: To implement.
#

# Verifier doesn't apply parent reductions locally in child contexts.
#test(U00){a:int => f(x:int):=x+1 => b:=f(a) => a=2 => b=3}        # Not automatically supported.
#test(S00){a:int => f(x:int):=x+1 => b:=f(a) => a=2 => {f(a),b=3}} # Supported when invoked explicitly.

# Variables.
#test(S00){x^:int=123,x^=123,x:=124,x^=124,x+=2,x=126}

# Variables and forks.
#test(S00){vs:=for(x:=1|2,y^:int=x){y},vs[0]<>vs[1]}
#test(S00){vs:=for(x^:int:=0,y:=1|2,x:=y){x},vs[0]<>vs[1]}

# Verifier and runtime must support arrays with unknown but increasingly lower bounded length.
#test(S00){Print of ns:=for(i:=0..9) do ns[i-1]+1 or 0}
#test(S00){Print of fs:=for(i:=0..9) do fs[i-1]+fs[i-2] or 1}
#test(S00){fs:=array{1,1}+for(i:=0..8) do fs(i)+fs(i+1)}

# Determine why this multiplies execution time so visibly.
#for{1|2|3|4}         # Slows down a lot due to commonly used equation pollution.
#for{456|457|458|459} # Slows down a bit.

# Proofs as programs.
# Show how we can construct converges proofs in imperative code by building up converges functions.
