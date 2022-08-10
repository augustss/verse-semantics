################################################################################
# Learn MaxVerse By Example
################################################################################

# We support line comments
<# Block comments #>
<# Block comments nest <# like this #> #>

# Constants
123         # integer
0xAB12      # hex integer
123.45      # rational
123.45f     # floating point (h, f, d, q)
'x'         # character
"ABC"       # string
"Hello\r\n" # string with backslashes

# Identifiers
Cos,actor                  # Ordinary identifier, may be value or type
MyString.Length            # Field access
(actor:)Health             # Disambiguate multiple-inheritance field

# Numeric operators
(2*a+3*b)/5

# Functions
f(x:int,y:int):=x*x+y*y     # Value only.
f(x&y:int):=x*x+y*y         # Same as the above.
f(x:int,y:int):int=x*x+y*y  # Type only.
f(x:int,y:int):int          # Abstract.
f(:int,:int):int            # Names are optional.

# Functions.
f of:
	x:int
	y:int
is:
	x+y

# Conditionals
if(x<10) then Print(x)
if(x<10) {Print(x)}
if(x<10). Print(x)
if(x<10).
	Print(x)
if(x<10)
then Print(x)
if:
	x<10
then Print(x)
if:
	x<10
then:
	Print(x)

# Arrays
Sum(xs:[2]int):=xs[0]+xs[1]
Sum(xs:[]int):=if(x:=xs[0],ys:=xs.Slice[1]) then x+Sum(ys) else 0
DownTo(n:int):=if(n=0) then array{0} else array{n}+DownTo(n-1)

# Macros
for(x:=0..99). Print(x)         # Mathematical style.
for(x:=0..99) {y:=x*x,Print(y)} # Braced style.
for(x:=0..99)                   # Multi-line braced style.
{
	y=x*x
	Print(y)
}
for(x:=0..99):                  # Significant indent style.
	y:=x*x
	Print(y)
for:
	x:=0..99
	y:=x*x
do:
	Print(y)

# Bubble sort example, from Wikipedia pseudocode.
BubbleSort(ref xs:[n]t where n:nat, t:type, Ordered[t]):=
    loop:
        var Swapped:logic=false
        for(i:=1..n-1):
            if(xs[i-1] > xs[i]):
                Swap{xs[i], xs[i-1]}
                set Swapped=true
    until not Swapped

# Advanced strings
let(Name:="Tim"). "Hello, {Name}."  # String interpolation
"Hello, "+name+"."                  # String concatenation
s.Length                            # String length
s.Slice[2,10]                       # String slice (may fail)

# String interpolation can contain arbitrary expressions.
Print("Hello, {Name}. You have {EmailAccount.Messages.Length} messages.")
Print("You have {n} message{"s" where n<>1}")

# Access specifiers.
Health<private>:int=3

# Effects specifiers.
f(x:int)<computes>:=x+1

# Attributes.
@edit_slider{0.0,1.0}
Health:float

@attr
f of:
	x:int<specifier> @attr
	y:int<specifier> @attr
is x+y#

# Documentation attributes.
@<doc;Add two numbers>
Add of:
	x:int @<doc; First number>
	y:int @<doc; Second number>
is:
	x+y


# Markup
<markup>Feeling <b>bold</b>?</markup> # HTML style.
<markup;Feeling <b;bold>?>            # Markdown style.
<list; ~Apples ~Oranges>              # List.
<markup:>                             # Significant indent.
	Indented style markup.
	Healthy foods include<List:>
		~Apples
		~Oranges
#<https://www.epicgames.com>           # Markup syntax assists to construct URLs.
<markup:>
	Markup supports arbitrary injection of code
	via string interpolation like {Name} or {0u1234}
	and also PHP style code injection.
    &for(i->Message:Messages) do <m:>
        Message {i}: {Message.subject}

# Paths (required for open-world module interoperability)
/UnrealEngine.com/Actor/Name   # Globally unambiguous path
(/UnrealEngine.com/Actor:)Name # Globally disambiguated symbol

# Units
Distance:=3.5m
Momentum:=3.5{kg*m/s}
Velocity:=vector3{1,2,3}{m/s}

# Choice forks.
for(k:=9,x:=0..k|100..100+k). Print(x)
f(x:int|string)=case(x):
	y:int    => Print(y)
	s:string => Print(s.length)

#
# Simple types and values.
#

A:int   = 123          # An integer.
B:float = 4.5          # A floating point number.
C:char  = 'X'          # A character.

D:string = "Hello"     # A string, which is just an array of characters.
E:[]int  = (3,5)       # An array.
F:[]any  = (3,"Hello") # Tuples are arrays too.
G       := ()          # The empty array

#
# Functions take a single parameter, which might be a tuple.
#

# Takes a parameter that is an integer, returns twice as much.
Twice0(x:int):=x*2

# Or you can use a return expression.
Twice1(x:int):=
	return x*2

# Takes a parameter that is a tuple of integers.
# Locally, we name the first component x, and the second y.
Add(x:int,y:int):=x+y

# Takes a pointer to an integer and increments the integer.
Increment0(ip:^int):void=
	ip^ := ip^ + 1
Increment1(ref i:int):void=
	set i = i + 1

#
# Functions that interact irreversibly with the outside world must say so.
#

PrintSum(x:int,y:int)<interacts>:=
	Print(x+y)

#
# Failure is control flow.
#

# Simple example.
PrintEquality(a&b:int)<interacts>:=
	if(a=b).
		Print("Equal")
	else.
		Print("Unequal")

# Success exposes bound variables.
PrintElement(xs:[]int,i:int)<interacts>:=
	if(x:=xs[i]) then.
		Print("Element {i} is {x}")
	else.
		Print("Out of bounds")

# Dependent types.
PrintElement(xs:[n]int,i:nat<n where n:nat)<interacts>:=
	Print("Element {i} is {xs[i]}")

# Use assert{expr} to shut up test that shouldn't fail.
PrintElementOrAssert(xs:[]int,i:int)<interacts>:=
	x:=assert{xs[i]}
	Print("Element {i} is {x}")

# "a and b" runs a then b, and produces b if both succeed.
PrintIfBothLessThanTen(a:int,b:int)<interacts>:=
	if(a<10 and b<10).
		Print(a,b)

# "a or b" runs a and produces it if it succeeds, else runs b.
PrintIfEitherLessThanTen(a:int,b:int)<interacts>:=
	if(a<10 or b<10).
		Print(a,b)

# "not a" runs a and fails if a succeeds, else it succeds and produces false.
PrintIfNegative(a:int)<interacts>:=
	if(not nat[a]).
		Print(a)

#
# Equality is unification a.k.a. pattern matching.
# 

# Equality a=b both compares and unifies.
ClampToZero(a:int):=
	x:int
	if(a>=0).
		x=a
	else.
		x=0
	return x

# Case statements do pattern matching.
SaySomethingAbout(x:int)<interacts>:void=
	case(x).
		i:int where int[i/2] => Print("It's an even integer")
		n:nat                => Print("It's a natural number")
		i:int                => Print("It's something else")

#
# Capturing failure: the logic type isn't quite like bool.
#

# Function ought to just fail instead of returning a logic value.
IsLessThanTen(a:int)<decides>:void=a<10
PrintNumbersLessThanTen(as:[]int)<interacts>:void=
	for(a:as, IsLessThanTen[a]):
		Print(a)

# But you can return a logic value if wanted. Then it's like:
IsLessThanTen(a:int):logic=logic{a<10} # Returns true or false.
PrintNumbersLessThanTen(as:[]int)<interacts>:void=
	for(a:as, IsLessThanTen(a)?): # Need '?' to ask whether the logic value is true
		Print(a)

#
# Mutables.
#

var H:int=3    # H is a unique pointer with initial value 3.
set H=7        # Set it to 7.
set H=H*2      # Multiply it by 2.
set H+=1       # Add 1.
ref I=H ref    # I is a pointer equal to the pointer H.

#	
# Power: Let's raise N to the power of E.
#

# Using mutation.
Power1(N:int,E:nat):int=
	var x:int=1
	for(i:=0..9).
		set x=x*e
	return x

# Recursive style, casting E-1 to natural number.
Power2(N:int,E:nat):int=
	if(E1:=nat[E-1]). N*Power2(N,E1) else. 1

# Recursive style: a or b produces a if a succeeds, else produces b.
Power3(N:int,E:nat):int=
	N*Power2(N,nat[E-1]) or 1

# Iterative style.
#Power4(N:int,E:nat):int=
#	last. P:int=1 where 1..E then P*E

#
# PrintPrimes: Let's print prime numbers.
#

# Brute force.
PrintPrimes0(n:nat)<interacts>:void=
	for(i:=2..n-1; j:=2..i-1; nat[i/j]). # Is the rational number i/j a natural number?
		Print("{i} is prime")

# Sieve of Eratosthenes with pointers.
PrintPrimes1(n:nat)<interacts>:void=
	var IsPrime[i:nat<n] : logic = true
	for(i:=2..n-1 where IsPrime[i]).
		Print(i)
		#for(j:nat=i*2 as j<n then j+i).
		#	set IsPrime[j] = false

# Pure Sieve of Eratosthenes using recursion.
Primes0(n:nat):[]int=Sieve(for{2..n-1}) where:
	Sieve(p:int,..qs:[]int):=(p,..Sieve(for{q:qs where not nat[q/p]}))
	Sieve():=()

# Pure sieve using fold iteration.
#Primes1(n:nat):=last{ps:[]int=() over p:=2..n-1 then ps+for{p where for(q:ps). not nat[p/q]}}
#Primes2(n:nat):=ps:=for{c:nat=0 over p:=2..n-1 then c+1 where for(q:=ps[0..c-1]). not nat[p/q]}

#
# Fibs: Let's produce an array of Fibonacci numbers.
#

# Recursively build up an array.
Fibs0(n:nat):=as where:
	as[i:=0..n-1] :int = as[i-1] + as[i-2] or 1

# Produce iteratively without side effects.
#Fibs1(n:nat):int=
#	for(a:int=1,b:int=1,i:int=0 as i<n then b,a+b,i+1). a

# Produce iteratively with pointers.
Fibs2(n:nat):int=
	var Fibs : []int = ()
	var a&b  : int = 1
	for(i:=0..n).
		set Fibs += array{a}
		set (a,b) = (b,a+b)
	return Fibs

#
# MakeString: Let's convert an integer to a string, with an optional base.
#

MakeString(A:int,?Base :nat<16 = 10):string=
	if(A>=0).
		Leading := floor(A/Base)
		Prefix  := MakeString(Leading>0,?Base:=Base) or ""
		Digit   := assert{"0123456789ABCDEF"[A-Leading*Base]}
		return Prefix + array{Digit}
	else.
		MakeString(-A)

#
# Optional type ?t.
#

# Use no:?int to declare an optional integer which is not an integer itself,
# but is either false or truth{i} for some integer i. Use no? to extract its integer.
PrintOption(no : ?int):=
	if(n:=no?) then.
		Print("Value is {n}")
	else.
		Print("No Value")
PrintOption(false)      # Empty option.
PrintOption(truth{123}) # Option containing 123.

# Create an option from an expression that may fail.
PrintIfPositive(n:int):=
	no := option{n where n>0}
	PrintOption(no)

#
# Working with tuples.
#

# A simple function taking multiple parameters.
MySum0(x:int,y:int,s:string) := x + y + s.length

# Tuple elements of the same type can be declared using x&y syntax.
AddPair1(x&y:int,s:string) := x + y + s.length

# Functions can return tuples.
Differences(x:int,y:int) : [2]int = (x+y,x-y)

# Tuples can be bound in a definition.
(Diff0,Diff1) := Differences(5,3)

#
# Iteration.
#

# Iterate and filter arrays.
PrintPositiveElements(as:[]int)<interacts>:=
	for(a:as, a>0).
		Print(a)

# Check all elements of string and succeed or fail.
IsIdentifier(cs:string)<decides>:=
	for(c:cs).
		 c=('a'..'z'|'A'..'Z'|'_')

# Verse provides the expressiveness of universal and existential quantifiers.
AreAllPositiveElementsEven(as:[]int)<decides>:void=
	for(a:as, a>0). int[a/2]
AreAllEvenElementsPositive(as:[]int)<decides>:void=
	for(a:as, int[a/2]). a>0

#
# String formatting.
#

# Inside a string, use {expr} to ToString an expression.
# If the overloaded ToString function for that type takes optional parameters,
# you can put them right there.
PrintStuff(i:int)<interacts>:=
	Print("Your number is {i}.")
	Print("Your number in hex is {i,?Base:=16}.")

# If you define a nominal type, you can overload ToString to print it nicely.
Point:=struct{x:int,y:int}
ToString(p:Point):string=
	"A point at {x} and {y}"

#
# Markup.
#

# You can recursively interleave code, strings, and markup like React or PHP.
# Anything inside {..} is converted to markup with Markupify if implemented, else ToString.
Message(Name:string) :=
	<Markup>
	Your name is {Name}.
	This is <B>bold</B>.
	&for(i:0..9). <Markup:>
		Here is the number {i*2-1}.
	</Markup>

#
# Structs are nominal types that don't support inheritance.
# They are comparable if all fields are comparable or any field is a unique pointer.
#

Coordinate:=struct:
	x:int
	y:int

Bounce(c:Coordinate, dx&dy:int):Coordinate=
	return Coordinate{x:=c.x+dx; y:=c.y+dy}

#
# Unions.
#

# Trees as parametric types.
tree0(t:type)=union.
	Leaf : t
	Node : [2]tree0(t)

# Let's construct some instances.
MyLeaf0 := tree0(int).Leaf(3)
MyTree0 := tree0(int).Node(MyLeaf0,MyLeaf0)

# We decompose unions using pattern matching and unification.
TreeSum(T:tree0(int)):int=case(T).
	Leaf[i:int]   => i
	Node(A:T,B:T) => TreeSum(A) + TreeSum(B)

# Trees as functors.
tree1(f:t->u where t&u:type)=union.
	Leaf : f
	Node : [2]tree1(f)
MyLeaf1  := tree1(int).Leaf(3)
MyTree1  := tree1(int).Node(MyLeaf1,MyLeaf1)

# Besides tree1(t) making a type, it also makes a functor from tree1(t) to tree1(t).
# Here we're making a new tree from MyTree1 with the elements incremented.
MyTree2 := tree1(x:int=>x+1)(MyTree1)

#
# Compile-time evaluation.
#

# What does the verifier know at compile-time?
M:=3    # Verifier knows x is exactly 3.
N:int=3 # Verifier approximates x as :int, but runtime knows it's 3.

# Verifying this is divergent.
#Factorial0(x:int):=
#	if(x>1) then x*Factorial0(x-1)
#	else         1

# Verifying this is convergent but approximated as :int.
Factorial1(x:int):int=
	if(x>1) then x*Factorial1(x-1) else 1

# Verifying this is convergent and exact.
# The "upon" keyword ensures we run the right-hand side if its condition
# is met, in this case if x is a precisely known integer.
Factorial2(x:int):int<known[x]> =
	if(x>1) then x*Factorial2(x-1) else 1

#
# Overloading.
#

# You can overload a function on distinct types.
Combobulate(x:int):int=x
Combobulate(s:string):int=s.length
TestCombobulate()<interacts>:=
	Print(Combobulate(123))   # Call with integer.
	Print(Combobulate("Bob")) # Call with string.
	#Print(Combobulate(4.5))  # Can't call with float because there's no matching overload.

#
# Advanced effects.
#

# A function without declared effects defaults to <transacts>,
# which is the same as <computes><varies><reads><writes>.
MyTransactionalFunction(ref ip:int) := let(i:=ip; set ip+=1). i

# A pure function has no side effects.
MyPureFunction(x:int)<computes>:=x+1

# Exclusive effects: converges and computes.
# Heap effects: varies, reads, writes.
# Exception effects: throws(type).
# Multiplicity effects: diverges, succeeds, fails, decides, resolves.

#
# Math
# 

# We have exact numeric types nat, int, rational written as 3, -3, 3.5r.
# They use variable-precision math but are allocation-free for small values.
# Hence there's no possibility of overflow.
# Division a/b produces a rational number, so use floor(a/b) to get an integer.

# We have floating point types with float=float64, written 3.0.

# Compute sum recursively. Pattern matches x and xs.
Sum(xs:[]int):int=case(xs).
	(x:int,..xs:[]int) => x+Sum(xs)
	()                 => 0

# Compute sum with computes iterative loop.
#Sum(xs:[]int):int=
#	last{sum:int=0 while x:xs then sum+x}

################################################################################
# ZZT/Rogue object oriented game example.
################################################################################

# Some constants and types.
BoardX := 80
BoardY := 24
screen := [BoardX][BoardY]char

#
# Our actor class hierarchy.
#

# Base class of all gameplay objects.
actor:=class:
    var x&y : int
    Model   : char
	Tick(Keys:[]char):void

# A player.
player:=class(actor):
	Model:='O'
	Tick(Keys:[]char):void=
		for(k:Keys).
			(dx,dy):=case(k).
				'W'|'w' => ( 0,-1)
				'A'|'a' => (-1, 0)
				'S'|'s' => ( 0,-1)
				'D'|'d' => ( 1, 0)
				:any    => ( 0, 0)
			if. MoveActor(A,dx,dy)

# An enemy.
enemy:=class(actor):
	Model:='X'
	Tick(Keys:[]char):void=
		dx:=nat.random(2)-1
		dy:=nat.random(2)-1
		if{MoveActor[A,dx,dy]} # Revert changes if fails due to collision.

#
# Engine implementation.
#

# The actor list.
var Actors:[]actor=array:
    player{x:=20,y:=12}
    enemy {x:=60,y:=12}

# Our advanced rendering engine.
Render():screen=
    for(y:=0..BoardX-1). for(x:=0..BoardY-1).
        if(a:Actors, a.x=x, a.y=y) then:
            a.Model
        else ' '

# Our leading-edge physics system.
MoveActor(a:actor,dx:int,dy:int)<decides>:void=
    set a.x = Clamp(a.x+dx,0,79)
    set a.y = Clamp(a.y+dy,0,24)
	not for(b:Actors<>a) do b.x=a.x and b.y=a.y

# World tick. We expect the framework to call this once per frame.
TickWorld(Keys:[]char):=
	for(A:Actors).
		A.Tick(Keys)

################################################################################
# Traits encompass Haskell type classes, C++ concepts, implicit parameters.
# What to look for: <trait>, <instance>.
################################################################################

# The <trait> keyword always precedes a function: ToString<trait>(:any):string.
# An <instance> follows the same pattern: ToString<instance>(c:char):=array{c}.
# The <trait> defines a type that each <instance> must belong to.
#    ToString refers to a function comprising the trait's visible instances.
#    ToString.type refer to the instance-independent type of the <trait>.
# Caller passes traits into functions as non-optional named parameters.
#    Normally, they are passed implicitly.
#    You write ToString(3,5) and the result is ToString(3,?Base:=10).
#    But the caller can provide explicitly like ?Base:=16.
# Dependency on a trait in an abstract context makes it an implicit parameter.
#    So ToString(3,5) has implicit ?Base because of ToString(xs:[]ToString).
#    This includes nested dependencies through nested abstractions.
#    This excludes non-abstract dependency like v in f(:int):int=v.
#    This excludes nested abstract dependency like v in f(g(v):int):int.
# When you write a function dependent on traits, your function doesn't see
#    the trait instances that exist where the function is defined. It sees
#    an abstract parameter belonging to the type in the <trait> definition.
#    So, it's possible ToString(foo) works outside the function but not inside.
# Referential transparency means referencing f doesn't bind its implicits.
#    But the eta conversion implicit in passing ToString to a function
#    that's expecting f(:t) causes its implicits to be caller-bound.
# Nomenclature is 'trait' because precise 'caller scoped identifiers' is too
#   jargonic; type classes is too overloaded a term since Verse has types
#   and classes; implicits is just a special case of traits; and concepts
#   are too general and un-Google-able a term.
# We express lawfulness (e.g. a proof of the reflexive property of addition)
#   through fields in traits like Reflexive(a&b:int)<converges>:=a+b=b*a.
#   We can cheat on these traits by making these 'proofs' non-total
#   like Transitive(a&b&c:float)<partial>:=assert{a+(b+c)=(a+b)+c}.
#   Because of this, we can have a single set of traits useful both in
#   provable theorems on e.g. int, and runnable programs on e.g. float
#   whose "proofs" aren't allowed in <converges> contexts.
# We should be able to overload a trait on both e.g. int and rational
#   as long as their implementations are pure and match. The most specific
#   "matching" one is selected by intersecting all of them through unification.
# There is pressure on functions to become urelements:
#   Store the function's trait type in the function value: ToString.type
#   Store a macro in the function value. f(t:type):=type{:t,:t}
#   Store static variables with open world extensibility in the function value:
#      c:=class{x:int,Make<static>(x0:int):=c{x:=x0}}
# Usage Cases
#	- module configuration parameters with overridable defaults
#	- additive, rings, fields, modules, vectorspaces -- for lawful numerics, and lawless symbolic manipulation.
#	- semiorders, partial orders, orders, lattices.
#	- showing values as strings. parameters: precedence, base, format, etc.
#	- parsing values as strings. parameters: precedence, base, format, etc.
#	- serialization
#	- functors, monads -- containers, functions, continuations
#	- iterators
#	- hashing
#	- programmer-friendly introspection of declared metadata based on syntactic paths
#	- SEE: Haskell Basic Algrbra Proposal, ftp://ftp.botik.ru/pub/local/Mechveliani/basAlgPropos/
#
# Compare to: Haskell and Scala typeclasses, C++ concepts, Rust traits.
#	Like Haskell typeclasses, there is a backtracking search to identify appropriate instances.
#	Like Haskell typeclasses, there is a passed-in dictionary providing scope, e.g. "f(x:int using show).
#	See also: https://hackage.haskell.org/package/implicit-params

#
# Simple example without traits, using named optional parameters.
#

ToString(n:nat, ?Base:nat>0<=16=10):string =
	if{ToString(Floor(n/Base)>0,Base)} + "0123456789ABCDEF"(n-m*Base)
ToString(i:int, ?Base:nat>0<=16=10):string = "-" + ToString(-i,Base)

#
# Simple example of a trait for an implicit parameter Base.
#

# Define Base's type and default value.
Base<trait>():nat>0<=16 = 10

# Define ToString, with nat and int overloads expecting trait Base.
ToString(n:nat where Base)<instance>:string = 
	let(D=Floor(n/Base())).
		if(D>0){ToString(D)} + "0123456789ABCDEF"(n-m*Base)
ToString(i:int where Base())<instance>:string = "-"+ToString(-i)
ToString(c:char):string = array{c}
ToString(ss:[]ToString)<instance>:string = Concatenation(ss)

# Calling ToString.
S:=ToString(123)             # Uses Base:=10 because that's the local version we see.
T:=ToString(123,?Base():=16) # Explicitly specify Base here.

# Defining functions with implicits.
# Any mention of Base in function domain (not range) directly or recursively through
# abstraction instantiation adds a named trait parameter propagating it, the name
# being the nominal path of that symbol's original <trait> definition.
F(x:int           ):=Print(ToString(x)) # Uses local Base.
G(x:int where Base):=Print(ToString(x)) # Uses passed-in Base.

#
# ToString as a trait.
#

# Definitions. Closed world unambiguity requires numbered invariant-domain overloads.
ToString<trait>(a:any):string
ToString<0>(n:nat):=todo
ToString<0>(n:int):=todo
ToString<1>(c:char):=array{c}
ToString<2>(ss:[]ToString):=for{::ss}

# Use.
A:=ToString(123)
B:=ToString(123,'X')

#
# Parametric traits.
#

<# We need to make some basic operators traits #>
operator'+'<trait>(x&y:any)<converges>:any
operator'-'<trait>(x&y:any)<converges>:any
operator'*'<trait>(x&y:t  )<converges>:any
prefix'-'  <trait>(x:any  )<converges>:any

<# Base definitions #>

# Additive and Ring traits.
Additive<trait>(t:type where f(x&y:t)=>t[x+y]):class:
	Zero:t
	Reflexive(x&y:t)<computes>:type{x+y=y+x}=assert{x+y=y+x}
Ring<trait>(t:type where f(x&y:t):=t[x-y] and t[-x]):class(Additive(t).type):
	Difference(x&y:t):=t[x-y]
	Negation(x:t):=t[-x]

# Integers are additive rings. Note that instance{..} just constructs an archetype to
# be passed into the <trait> definition's class instantiation expression above.
Additive<instance>(int):=instance {Zero:=0}
Ring    <instance>(int):=instance {}

# Rationals are additive rings.
Additive<instance>(rational):=instance {Zero:=0}
Ring    <instance>(rational):=instance {}

<# Usage #>

# Local binding.
f(x&y:int):=x+y

# Caller binding, like Haskell typeclasses.
f(x&y:t where t:type,Additive[t]):=x+y

#
# Related traits.
#

point:=struct {X&Y:rational}
(S:rational)*(P:point):=point {X:=S*P.X,Y:=S*P.Y}

Field<trait>(t:type where f(x&y:t):=t[x*y]):class(Ring(t)) {}
VectorSpace<trait>(s&v:type where Field[s]):class(vector_space(s,v).type) {}

Field<instance>(rational):=instance(Ring(rational).type) {}
VectorSpace<instance>(rational,point):=instance {}

#
# The evolutionary challenge:
# When we reference a set of overloaded functions f, we need to produce a
# single open-world-safe stub function that can dynamically dispatch
# to the proper overload based on the parameter. This relies on trait
# instances being numbered and each individually invariant in their domains.
# This way, our stub can freeze the precise set of overloads needed.
#

# V0.
M0:=module.
	f(0):=1
M1:=module.
	f(2):=3
M2:=module.
	F(G(:t):int,x&y:t where t:type):t=G(x)
	X:=using(M0,M1). F(f,0,2)

# V1. Shows M2's reference to f must bundle up logic to statically dispatch.
M0:=module {f(x:int):=1}

#
# On exposing the core structure of mathematics.
# We express non-canonical things without caller scoped identifiers.
#     Examples: SemiGroup, Group, AbelianGroup
#     We do this when there isn't a preferred group on e.g. the integers.
# We express canonical things with caller scoped identifiers.
#     Example: Additive (Group), Multiplicative (Group), Ring, Field, Module, VectorSpace.
#

#
# Implicits with .symbol overloading.
# 

Displayable<trait>(t:type):class.
	Display():string

actor:=class.
	Name:string
	Health:int

Displayable(actor)<instance> := instance.
	(A:actor).Display() := Print("I'm {A.Name} and I have {A.Health} health.")

A:=actor {Name="Bob"; Health:=123}       # Make an actor.
A.Display()                              # Display it the nice way.
Displayable(actor).operator'.Display'(A) # Display it the complicated way.

DisplayTwice(A:t where t:type,Displayable[t])<interacts>:=
	A.Display()
	A.Display()

#
# Interfaces are simpler when there's a fixed relationship between types.
#

displayable:=class.
	Display()<interacts>:void

actor:=class(displayable).
	Name:string
	Health:int
	Display():=Print("I'm {A.Name} and I have {A.Health} health.")

A:=actor {Name="Bob"; Health:=123} # Make an actor.
A.Display()                        # Display it the nice way.

DisplayTwice(A:displayable)<interacts>:=
	A.Display()
	A.Display()

################################################################################
# Verse AMA.
################################################################################

# Why failure?
if(i>=0 and i<as.length) {printf("%i",as[i]);} # C++ style, can go wrong.
if(a:=as[i]). Print(a)                        # Verse style.
if(a:=as[i]). Print(a) else {}                # Equivalent to the above.
Print(as[i])                                  # Error.

if(c:=as[i]+bs[i]). Print(c) # Lots of failing expressions can exist together.

if((a:=expr1) and (b:=expr2)). todo # You can write this.
if(a:=expr1,b:=expr2). todo         # But this is simpler and equivalent.

# We can abstract over expressions that succeed or fail by bottling
# them up into option values like ?int and logic values.

option{1}   # Produces just that.
option{1=2} # Produces false.

logic{1}    # Produces true.
logic{1=2}  # Produces false.

o:?int  => if(a:=o?). Print(a)
o:logic => if(b?). Print("True") else Print("False")

# Structure code to enable the compiler to catch as many potential
# problems as possible at compile-time, such as accidental failure.
if(a:=as[i],c:=a*a+b*b). Print(c)
if(a:=as[i]). let(c:=a*a+b*b). Print(c)

assert{expr} # Shuts up failure. Equivalent to:
if(expr). Err("assertion failed")

# Can this public module function evolve over time without breaking?
f(x:int):=3              # this function must always return 3
f(x:int):=Err("Oh noes") # except it can evolve into this

f(x:int):int   # yes, because effects are <transacts>
f(x:int):int=3 # this can evolve into...
f(x:int):int=4 # this.

# The evolution of types in Verse code. ShipVerse:
vb:=struct{Vertices:[]vertex,Indices:[]int}
Flatten1(VB:vb):=for(i:VB.Indices). Vertices[i] # Compiler gives an error
Flatten2(VB:vb):=for(i:VB.Indices). assert{Vertices[i]} # ShipVerse accepts.

# In MaxVerse, use dependent types.
vb:=struct{Vertices:[n]vertex,Indices:[]nat<n where n:nat}
Flatten3(VB:vb):=for(i:VB.Indices). Vertices(i) # MaxVerse accepts.

# There are two versions of function calling and array access.
f[a] # if calling f with a fails, we fail here (if this context is allowed to fail)
f(a) # if calling f with a fails, produce an error right now

# This is helpful in catching "unintended failure".
if(a:=as[i],b:=f(a),b<10). todo

for(x:xs,x<10). Print(x)
for(x:=1..10). Print(x)

for(x:=1..10,y:as,z[x],w:y,Factorial[q:qs],q<x). Print(x)
for(xs:xss,x:xs). Print(x)

for(a:=as[i:int]). Print("Element {i} is {a}")
for(a@ii:as). Print("Element {i} is {a}")

# Are all elements x in xs where x is less than 10, all prime numbers"
if(for(x:xs,x<10). IsPrime[x]) then todo else todo

# All 8-bit binary numbers.
for(Digits:=for(n:=8,i:nat<n). 0|1). Print(Digits)
