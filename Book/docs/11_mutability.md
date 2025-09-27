# Mutability

Verse is unlike many languages you may be familiar with when it comes to mutable data structures.

## Pure computations

A lot can be done in the *pure* fragment of the language where neither variables nor data structures are modified after being created. The following shows examples of values, structures and classes. Recursive data requires classes (structs cannot be self-referential). Use `?T` (option type) to terminate recursion:

```
X :int= 3     # Explicit typeY := 42       # Type is inferred from RHS
X+39=Y

pt := struct{ X:int=0, Y:int=0 }
P1 :pt= pt{X:=1}		# Initialization to X=1,Y=0
P2 :pt= pt{Y:=0;X:=1}	# Same in different order
P1=P2				# Both values are identical

list := class{ I:int=0, N:?list=false }
L1 := list{ I:=1, N:=option{list{}} }
L2 := list{ I:=1, N:= L1.N }
L1 = L2			# Compile error: no built-in equality for classes
```

As there is no built-in comparison operator for classes (the `unique` specifier is left for later, but it would not help here), define your own pure comparison. Mark it `<computes>` to indicate purity (and to retract `no_rollback`):

```

list := class:
I:int=0
N:?list=false
Compare(O:list)<computes><decides>:void=
	Self.I=O.I
    (not Self.N? and not O.N?) or Self.N?.Compare[O.N?]

L1 := list{ I:=1, N:=option{list{}} }
L2 := list{ I:=1, N:= L1.N }
L1.Compare[L2]	# Yes!

L2 := L1
L2.Compare[L1]	# Obviously

L3 := list{ I:=1, N:=option{L1} }
L2.Compare[L3]  # True
```

Notice the use of square brackets and `<decides>`. The function checks isomorphism: instances have the same shape and all primitive values are identical.

## Introducing side effects with var and set

Things get interesting when side effects are allowed. Rule of thumb: any value reachable from a variable annotated `var` requires a `<reads>` effect specifier and any use of `set` requires `<writes>`. For primitive types, things are still simple. A mutable variable is declared with a `var` annotation:

```
var X:int= 4	# Type is required for mutable variables
set X = 42  	# Update
var Y:int= X	# Copy
X=Y		# Sure!
set Y = 0	# Change Y but not X
X<>Y		# Ok
```

When we copy `X` into `Y` the two variables have the same value, but they can be updated independently to hold different values.

## Deep mutation of containers and structs

A variable of `struct` type that is annotated `var` is *deeply mutable*, this means that not only can one assign to the variable but also to fields of the structure it denotes\!

```
pt := struct<computes>{ X:int=0, Y:int=0 }
P1 :pt= pt{}
var P2 :pt= pt{}
set P2 = P1			# create a copy of P1
set P2.X = 123		# P2 is mutable
P1<>P2				# Not the same
```

Notice the `<computes>` on the declaration of the `struct,` it is required for historical reasons.  
Mutability is not limited to the direct fields of a structure, it extends to fields of nested structures as well. 

```
cpt := struct<computes>{ Col:string="R", Pt:pt= pt{} }
var C1 :cpt= cpt{}
var C2 :cpt= C1
C1 = C2			# Same
set C2.Pt.X = 123		# C2 is deeply mutable
C1<>C2				# Not the same
```

Copying from one variable to another entails a deep copy of the structure. Thus `set C2 = C1` means that `C2` has a mutable copy of `C1` – changes made to one of the structures are not mirrored in the other.  Containers such as arrays and tuples behave in the same way with respect to `var` and `set`.

```
I1:[]int= array{1,2,3}
var I2 :[]int= I1
set I2[0] = 42
I1 <> I2
```

What to make of this? First it shows that Verse has a strong notion of immutability, a value that is held in an immutable variable will remain immutable even if passed as argument to a function that chooses to move that value in a mutable variable. A corollary of this observation is that one may think that a language implementation must copy values frequently – in theory, yes, but in practice language implementers are wily and find ways to avoid many unnecessary copies.

## Classes and their mutability

It is noteworthy that structures do not allow fields that are declared as `var`; it is a limitation that has roots in the language implementation. It is not the case for classes, their fields can be mutable. One can define a point class that is similar to the `pt` structure above, but with mutable fields (and does not require `<computes>`). These fields can be updated even if the variable is immutable and when copying the value we share the same mutable fields. 

```
ptcl := class{ var X:int=0, var Y:int=0 }
P1 :ptcl= ptcl{}P2 :ptcl= P1       	# Copies P1 to P2
set P2.X = 42	      	# P2 is immutable, but P2.X can be udpated
P1.X = P2.X   	# The same!
```

Observe that, unlike with `struct`s, the `X` field is shared by `P1` and `P2`, updates to `P1` are visible from `P2`.  
Instances of classes cannot be compared directly (which is why the above looks at fields) in general. One way around that is with the `<unique>` class specifier.  Instances of `<unique>` classes can be compared for identity.

```
uptcl := class<unique>{ var X:int=0, var Y:int=0 }
P1 :uptcl= uptcl{}P2 :uptcl= P1      	# Same object
set P2.X = 42	      	
P1 = P2   		# The very same!
```

In the case of classes, annotating a variable that holds an instance with `var` simply makes that variable writable but does not change the mutability of fields as it does with `struct`s. So a non-`var` field of a `struct` type is always immutable, and a `var` field is always mutable.

```
nowrite := class{ Pt:pt= pt{} }   # A class with an immutable point
var N1 :nowrite	# N1 is var
set N1 = nowrite{}  # Yes!
set N1.Pt = pt{}  	# No! Compile error

write := class { var Pt2:pt= pt{} }  # A class with a mutable point
N2 :write= write{}	# N2 is not var
set N2.Pt2.X = 3	# Allowed
```

In the above `N1` is declared `var` but it is not allowed to update field `Pt`. On the other hand, `N2` is non-`var`, yet we can write to `Pt2`.  
