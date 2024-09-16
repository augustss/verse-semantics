########################################################################################################################
# Native symbols hardcoded in Verse.cpp native.
########################################################################################################################

# TODO: Refactor into native bootstrap VSON, native bootstrap verse, core VSON, core Verse.

false:=array{}
any(x:=_):=x
void(_):=false

comparable:type
rational:type
int:type
integer:=int
nat:type
natural:=nat

char8:type
char:=char8
char32:type
float32:type
float:type
float64:=float

known(C:comparable)<resolves>:type{a}

Length(as:[n]any where n:nat)<invariant>:=n

-(x:int    ):int
-(x:float32):int
-(x:float64):int

+(x:rational):=x
+(x:float32 ):=x
+(x:float64 ):=x

operator'+':any
operator'-':any
operator'*':any
operator'/':any
Throw(C:comparable)<contradicts><throws(type{C})>:false
Err(S:string)<diverges>:false
Concatenate(xss:[][]t where t:type):=for{x::xss}
Print(C:comparable):string
((&S:syntax)..(&T:syntax)):=Err("TODO")

Diagnose(a:any):void
Trace(a:any):void

converges&diverges&fails&computes&succeeds&decides&resolves&abstracts&iterates:fx
reads&writes&varies&transacts&suspends&interacts:fx
throws(t:type):fx

open&closed:vx

macro:type
array:macro
let:macro
operator'where':macro
if:macro
first:macro
for:macro
forall:macro
case:macro
function:macro
prefix'not':macro
type:macro
assert:macro
prefix':':macro
operator'or':macro
operator'and':macro
operator':=':macro
operator'|':macro
operator'<>':macro
operator'.':macro
operator'=>':macro
operator'=':macro
operator'?':macro

test:macro
assume:macro
verify:macro
allow:macro
expect:macro
reject:macro

########################################################################################################################
# VSON value subset of Verse.
########################################################################################################################

# Basic values.
false         := set{}      # Empty set, empty array, uninhabited type.
true          := set{false} # Set containing only the empty set.
logic(o:?any) := if(o?) true else false # The type containing just false and true.

# Macros supported in VSON.
array & map & truth & set : macro
instance                  : macro

# Markup tags, specificlly brought into <expr:markup>.
# To avoid symbol proliferation, it's implicitly <using Markup. e: text>, where identifier ambiguity 
# can be resolved with (local:)b and Markup.b.
Markup:=module.

    # Base type.
    m              := m
    markup         := class {Content:[]Element}
    element        := case  {char,markup,[]element,^element}

    # Standard markup.
    a  & anchor    : subtype(markup)
    b  & bold      : subtype(markup)
    blockquote     : subtype(markup)
    center         : subtype(markup)
    cite           : subtype(markup)
    delete         : subtype(markup)
    document       : subtype(markup)
	doc            : subtype(markup)
    font           : subtype(markup)
	frame          : subtype(markup)
    h1 & h2 & h3   : subtype(markup)
	h4 & h5 & h6   : subtype(markup)
    i  & italic    : subtype(markup)
    image          : subtype(markup)
    indent         : subtype(markup)
    insert         : subtype(markup)
    left           : subtype(markup)
    list           : subtype(markup)
    numbered       : subtype(markup)
    pre            : subtype(markup)
    right          : subtype(markup)
    strike         : subtype(markup)
    sub&sup        : subtype(markup)
    string         : subtype(markup)
    table          : subtype(markup)
    title          : subtype(markup)
    q  & quote     : subtype(markup)
    u  & underline : subtype(markup)

    # Formats.
    base64         : subtype(markup) # <base64:abcdef> produces []{0..255} supporting both A-Za-z0-9+/ and A-Za-z0-9-_ 
	                                 # and optional = end padding ignoring spaces and newline, like JavaScript atob&btoa.
    data           : subtype(markup) # <data:text/plain;base64,SGVsbG8sIFdvcmxkIQ%3D%3D>, https://en.wikipedia.org/wiki/Data_URI_scheme

    # Uniform resource names.
    urn            : subtype(markup) # uniform resource name, top of hierarchy; e.g. <urn:isbn:..>, <urn:uuid:..>.

    # Protocols.
    file           : subtype(markup) # <file:C:/dropbox/notes.txt> local file
    date           : subtype(markup) # <date:1985-04-12T23:20:50.52Z> extended rfc3339 and https://en.wikipedia.org/wiki/ISO_8601
    time           : subtype(markup) # <time:23:20:50.52Z>            extended rfc3339 and https://en.wikipedia.org/wiki/ISO_8601
    http           : subtype(markup)
	https          : subtype(markup)
    mailto         : subtype(markup)
    tel            : subtype(markup)
    geo            : subtype(markup)
    git            : subtype(markup)
    ssh            : subtype(markup)
    ftp            : subtype(markup)
    telnet         : subtype(markup)
    news           : subtype(markup)

# Units. Implicitly 1.0m translates to using(/Verse.org/Units){units'm'(1.0)} including user scope.
Units:=module.
    units'nm'  & units'um' & units'mm' & units'cm' & units'm' & units'km'  : reserved # Distance
	units'in'  & units'ft' & units'yd' & units'mi'                         : reserved # Distance
    units'ns'  & units'us' & units'ms' & units's'                          : reserved # Time
	units'min' & units'h'  & units'd'  & units'mo' & units'yr' & units'Hz' : reserved # Time
    units'ng'  & units'ug' & units'mg' & units'cg' & units'dg' & units'g'  : reserved # Mass
	units'kg'  & units'lb'                                                 : reserved # Mass
    units'B'   & units'KB' & units'MB' & units'GB' & units'TB' & units'PB' : reserved # Data size
    units'K'   & units'C' & units'F'                                       : reserved # Temperature
    units'ha'  & units'ac'                                                 : reserved # Area
    units'L'   & units'gal'                                                : reserved # Volume
    units'PA'                                                              : reserved # Pressure
    units'cd'                                                              : reserved # Brightness
    units'N'                                                               : reserved # Force
    units'J'                                                               : reserved # Energy & Work
    units'W'                                                               : reserved # Power
    units'%' := units'pct'                                                 : reserved # Percent
	units'em' & units'ex' & units'pc' &units'pt' & units'px'               : reserved # CSS units other than above
    units'i' & units'j' & units'k'                                         : reserved # Complex numbers and quaternions
    units'USD' & units'EUR' & units'JPY' & units'GBP' & units'CAD'         : reserved # Currency
	units'CHF' & units'RUB' & units'CNY' & units                           : reserved # Currency, more via /urn/iso/std/iso4217/CNY

# Constants.
NaN:=0.0/0.0
Inf:=1.0/0.0

# Numeric operations supported on constants in VSON.
prefix'+'   (x:rat|float32|float64                 )<total>:=x
prefix'-'   (x:t where t:=rat|float32|float        )<total>:t
operator'+' (x&y:nat                               )<total>:nat
operator'+' (x&y:int                               )<total>:int
operator'+' (x:t,y:u where t&u:=rat|float32|float64)<total>:promote(t,u)
operator'-' (x&y:int                               )<total>:int
operator'-' (x&y:rat where t&u:=rat|float32|float64)<total>:rat
operator'*' (x&y:nat                               )<total>:nat
operator'*' (x&y:int                               )<total>:int
operator'*' (x:t,y:u where t&u:=rat|float32|float64)<total>:promote(t,u)
operator'/' (x:rat,0                               )<total><fails>:false
operator'/' (x:t,y:u where t&u:=rat|float32|float64)<total>:promote(t,u)
operator'[]'(e:nat,x:nat                           )<total>:nat
operator'[]'(e:nat,x:int                           )<total>:int
operator'[]'(e:int,x:rat                           )<total>:rat
operator'[]'(e:t,x:t where t&u:=rat|float32|float64,t<>rat||u<>rat)<total>:promote(t,u)

########################################################################################################################
# Basic types.

# Basic types.
any              := typedef {x:=x}
logic            := typedef {:true|false}
void(x:any)      := false
type(f:function) := typedef<pure>{x:=f[x]}
comparable       :  type
reserved         :  type
type(f(:false):any where id(i->x:f)<resolves>:=i=x)<closed>:=id

# Mathematical numbers.
real                         : reserved # Persistent constructive reals.
nat  & int   & rational      : type     # Exact numbers.
int8 & int16 & int32 & int64 : type     # Storage types.
nat8 & nat16 & nat32 & nat64 : type     # Storage types.

# Floating point.
float                        : type     # IEEE floating point number. TODO: Decide float=float64, or traditional float&double.
float32 & float64            : type     # Storage types.
float16 & float128           : reserved # For later expansion.
float(a:float64)<total><invariant>:=a
float(a:float32)<total><invariant>:=float64.from(a)

# Text.
char8     := struct  {CodeUnit:nat<256}
char16    := struct  {CodeUnit:nat<0x10000}
char32    := struct  {CodePoint:nat<0x10FFFF}
ascii     := type    {C:char8 where C.CodeUnit<128}
char      := char8
string    := []char

# Markup.
markup    := Verse.Markup

########################################################################################################################
# Table types and functors.

typedef{&..ys:[]syntax}:=..
# TODO: separate macro namespace for type vs typedef.

# Table functor.
[t:comparable](u:type)<total>:type

# Array functor.
[n:nat](f:function ):=type.               for(i:nat<n). :f # Fixed size.
[     ](f:function ):=type. let(n:nat  ). for(i:nat<n). :f # Variable size.
*      (f:function ):=type. let(n:nat  ). for(i:nat<n). :f # Variable size.
+      (f:function ):=type. let(n:nat>0). for(i:nat<n). :f # Size>0.
tuple(fs:[]function):=type. for(f:fs). :f

# Set functor.
set(t:comparable)[k:t]<decides>:type{k}

# Option functor.
?(f:function):=type {option{:f}}

# Negation functors.
-(f:function)              := type {x   :any where !f[x]}
 (f:function)-(g:function) := type {w->x:f   where !f[w]}

# Get the idempotent type part of a function.
(f:function).type:=type {x:=f[x]}

# Function space functor.
((f:function)->(g:function))[h:function]<undecidable>(x:any)<undecidable>:=g[h[f[x]]]

# Types of types.
supertype(f:function)[g:function where v[x:f]:=g[x]]:=v # test domains and compose g of f, over domain of f.
subtype  (f:type)[g:type where v[x:g]:=f[x]]:=v         # test domains and compose f of g, over domain of g.

########################################################################################################################
# Macros.

# Internal.
operator'.' : macro

# Table forming macros.
option{&S:syntax}     := if(A:=&S) then set{A} else false
logic {&S:syntax}     := if(   &S) then true   else false

# Extraction macros.
(ao:?t where t:type)? :=  :ao
(ao:?t where t:type)! := !:ao

# Function macros.
function(&S:syntax)             {&T:syntax} : macro
function(&S:syntax)<(&R:syntax)>{&T:syntax} : macro
case(&S:syntax)                 {&T:syntax} : macro
case(&S:syntax)<(&R:syntax)>    {&T:syntax} : macro
case                            {&T:syntax} : macro
case<(&R:syntax)>               {&T:syntax} : macro
(&S:syntax)                  => (&T:syntax) : macro
(&S:syntax)<(&R:syntax)>     => (&T:syntax) : macro

# Scoping functions.
import{&S:syntax} : macro # Import a module.

# Scoping macros.
let             : macro
do              : macro # A locally scoped block whose contents can't be accessed outside. scope? block? try? do?
operator'where' : macro
embargo         : macro # Embargo a default-imported module, similar syntax to import.
using           : macro # Make visible using{p,q} in current scope, using(p){a} delimited.

# Simple iteration
while           : macro # Loop with just leading condition.
loop            : macro # Loop without leading condition.

# Definition macro.
((&syntax.identifier[S :string  ])       := &V:syntax) : macro
((&syntax.list      [SS:[]syntax])       := &V:syntax) := ..
((&S:syntax) -> (&T:syntax)    )         := &V:syntax) : macro
((&S:syntax) &  (&T:syntax) .. )         := &V:syntax) := ..
((&S:syntax) :  (&T:syntax)              := &V:syntax) := (&S):=(:&T):=(&V)
(            :  (&S:syntax)<(&U:syntax)> := &V:syntax) := ..
(            :  (&S:syntax)              := &V:syntax) := ..
(? (&S:syntax)                           := &V:syntax) := (&S):=option{&V}
# TODO: x?=expr,f(a)=expr, f[a]=expr, f(a)<x>=expr

# Verification macros.
# Static verification is succeeds{expr}, etc.
ensure      : macro    # runtime check, suppress failure
assert      : macro    # runtime check, "debugging only", whatever that means
test        : macro    # for compiler testing: verify that compiling the given code produces the given error message

# Verifier testing macros.
assume      : macro
allow       : macro
expect      : macro

# Macro helpers.
macro_error & macro_allow_effects : reserved

# Functional Logic Macros.
:(&S:syntax)               := i->o = (&S)[i]
!(&S:syntax)               :  macro # Similar to if(&S) then false? else false, but needn't fork.
(&S:syntax) && (&T:syntax) := if(  &S)        then &T     else false?
(&S:syntax) || (&T:syntax) := if(A:=&S)       then A      else false?
(&S:syntax) <> (&T:syntax) := if(A:=&S,!A=&T) then A else :false # May support inequality constraints later.
(&S:syntax) |  (&T:syntax) :  macro
(&S:syntax) .. (&T:syntax) :  macro

# The most general form of iteration.
iterates(&S; &T) {&U} catch{&V}          : macro # Primitive.

# Iteration macros.
if(&S:syntax){&T:syntax} else{&U:syntax}:=
	a->b:=iterate(i:=0; &S). a~>b=&T
	catch.                   a~>b=&U
if{&s:syntax}            else{&u:syntax} := if(S:=&s) {S } else false
if(&s:syntax){&t:syntax}                 := if(   &a) {&t} else false
if{&s:syntax}                            := if(S:=&s) {S } else false

for(&s:syntaxes){&t:syntaxes}:=
	as0->bs0:=let(n:=Length[as0]=Length[bs]). iterate((i,as,bs):=(0,as0,bs0); &s).
		as=(a,as1:any) && bs=(b,bs1:any) where a->b:=&t where continue {i+1,as1,bs1}
	catch. as=bs=() && n=i
for{&s:syntax}                           := for(S:=&s) {S}

first(&s:syntax){&t:syntax}              := if(&s){&t} else false?
first{&s:syntax}                         := find(S:=&s) {S}

last(&s:syntax){&t:syntax}               := 
    a->b:=iterate(r:=false; S:=&s). continue{truth{S}}
	catch. r?
last{&s:syntax}                          := last(S:=&s) {S}

all(&s:syntax){&t:syntax}                := if(S:=&s) {(&t) where next{S}} else :any
all{&s:syntax}                           := all(S:=&s){S}

map   (&s:syntax){&t:syntax}             := if(S:=&s) {map{A=>&t  ,..next{S}}}  else map{}
set   (&s:syntax){&t:syntax}             := if(   &s) {map{B:=&T=>B,..next{A}}} else set{}
option(&s:syntax){&t:syntax}             := if(S:=&s) {truth{S}}                else false
truth (&s:syntax){&t:syntax}             := if(S:=&s) {truth{S}}                else Refuse("truth")

#real.sum
#real.product
#t.intersection
#t.union
#t.min
#t.max

# Exceptions.
try                                      : macro

# Error at top level.
prefix'return' & prefix'yield' & prefix'break' & prefix'continue' & prefix'next' : macro
prefix'..' & prefix'%' & operator'&'                                             : macro

########################################################################################################################
# Attributes.

# Kinds of attributes.
effect & variance & visibility                    : type

# Effects attributes.
total & pure                                      : effect
allocates & reads & writes & points & transacts   : effect
errs & suspends & interacts                       : effect
diverges & succeeds & fails & decides & evaluates : effect

# Function variance attributes.
open&closed                                       : variance

# Visibility attributes, applicable to:
# nominal fields      x<private>:int
# stage               x:int<private>=3 # Parenthesize if necessary, like x:(?t)<private>=7
# pointer access via stage x:reads(int)<internal>:points(int)=3
# effects             f()<transacts>:int<internal>=reads{:int=3}
public & internal & private & closed              : visibility
scoped{&s:syntax}                                 : visibility
upon{&s:syntax}                                   : visibility

########################################################################################################################
# General Functions.

# Exception functions.
Throw(x:any)<throws(type{x})>:false

# Comparison functions.
((a:any) =  (b:any))<undecidable> : type{a}<undecidable>
((a:any) <= (b:any))<undecidable> : type{a}<undecidable>
((a:any) <  (b:any))<undecidable> := a where a<=b && a<>b
((a:any) >  (b:any))<undecidable> := a where b<=a && a<>b
((a:any) >= (b:any))<undecidable> := a where b<=a

# Functional logic refinement.                
known(x:any)<undecidable>:type{x}  # Succeeds when argument recursively constant, stalls otherwise.

# Functions.
((a:any)>>(f:function))<evaluates>:=f[a]

########################################################################################################################
# Math Functions.

# Promotion in numeric ops.
promote<private>(t&u:=double|float|rat):=if.
    t|u=double => double
    t|u=float  => float
    :any       => rat

# Bit operations.
(Min&Max           )(x:t,y:t where t:=nat|int|rat|float|double)<total>:t
(BitAnd&BitOr&BitXor&ShiftLeft&ShiftRight)(x:t,y:t where t:=nat|int)<total>:t
(BitScanForward&BitScanReverse)(x:int)<total>:nat
#not(x:int)<total>:int

# Math.
Quotient&Mod(D:int,d:int)<converges><decides>:=let(d<>0) in :int<succeeds>
Divide(D:int,d:int)<converges><decides>:=let(d<>0) in struct{Quotient:int,Remainder:nat<d where Quotient*d+Remainder=D}
Abs(n:rational):=if(n<0) -1 else if(n>0) 1 else 0
Clamp(a&b&c:rational):=..

# Conversions.
Float  (:rat|:double|:float)<total>:float
Float64(:rat|:double|:float)<total>:float64
Float32(:rat|:double|:float)<total>:float32
(Floor&Ceil&Round&Int)(x:t where t:=float|double|rat)<total>:int

# Special functions on float & double. Impure because we don't guarantee reproducibility across platforms.
(Sqrt&Exp&Ln&Sin&Cos&Tan&ArcSin&ArcCos&ArcTan&ArcCos&Sinh&Cosh&Tanh&ArSinh&ArCosh&ArTanh)(a:t where t:=float|double)<impure>:t

# Constants.
PiFloat : float
Pi      : real # Real constants. Use to_float(pi) and related to get approximations.

########################################################################################################################
# Table functions.

# Table functions.
(d:[comparable]any).keys:=for(k->v:d){k}
SortKeys(at:[t]u where t&u:type):[t]u

# Array operations.
(as:[n:nat]any).Length  := n
(as:[n:nat]any).Reverse := for{i:=1..n; as[n-i]}
(as:[n:nat]any).End      [i  :int           ]<decides>:=as[n-i]
(as:[n:nat]any).Slice    [i  :int           ]<decides>:=for(k:=i..n-1). a[k]
(as:[n:nat]any).Slice    [i&j:int where i<=j]<decides>:=for(k:=i..j-1). a[k]
(as:[n:nat]any).EndSlice [i&j:int where i<=j]<decides>:=for(k:=i..j-1). a[n-k]
(as:[n:nat]any).EndSlice [  j:int           ]<decides>:=for(k:=n..j-1). a[n-k]
Sort(at:[]t where t:type):[]t

# Options and logic.
#and(ao&bo:?any):=option{ao? && bo?}
#or (ao&bo:?any):=option{ao? || bo?}
#xor(ao&bo:?any):=option{ao!&&bo? || bo!&&ao?}
#not(ao:?any)   :=option{ao!}

########################################################################################################################
# Mutable variables.

# v:^int is an int variable, v^ reads it, v:=3 writes it.
(^(t:type))<total>                     : type
(x:^t where t:type)^<read>             : t
new(t:type    )<total>(x   :t)<impure> : ^t
new(f:function)<total>(x->y:f)<impure> : ^f.type

# Assignment destructuring.
operator':='(x:^t,y:t where t:type)<heap> : macro
# x->xm:=y, xo?:=y, (pm,qm):=(qm^+pm^,pm^), ??

# Derived operations. Need typeclass-like features to reason about range validity.
((ap:^t) += (b:any) where c:=ap^+b)<undecidable>:=ap^:=c
((ap:^t) -= (b:any) where c:=ap^-b)<undecidable>:=ap^:=c
((ap:^t) *= (b:any) where c:=ap^*b)<undecidable>:=ap^:=c
((ap:^t) /= (b:any) where c:=ap^/b)<undecidable>:=ap^:=c

########################################################################################################################
# Asynchronous.

# Asynchronous programming.
future(t:type)   : type       # An explicit future. It's an intensional singleton set whose value becomes available later.
spawn            : macro      # Start running suspending code returning a t here without suspending, returning a future(t).
await(:future(t))<suspends>:t # Awaiting a future(t), producing t latently.
race             : macro      # Run a bunch of suspending functions and when one completes, stop all of the others that are suspended.
when             : macro      # when(x:fu){actions} runs the actions when the future becomes ready. Blocks success while suspended.

########################################################################################################################
# Nominal types.

# Nominal Type Formers.
class       : macro    # Classes with inheritance.
struct      : macro    # A final class with no bases.
module      : macro    # A module, like an instance of a class.
union       : macro    # A nominal tagged union
instance    : macro    # An archetype that can be recursively intertwined with and input into a class to initialize its fields.
enum        : macro    # An enumeration.

# Virtual Class Formers.
subclass{&e:syntax}            :subtype(t0):=t0 where t0:=class   {e}
subclass(&t:syntax){&e:syntax} :subtype(t0):=t0 where t0:=class(t){e}

# Nominal Type Attributes.
final       : reserved # Can't have subclasses.
sealed      : reserved # Can't add fields.
concrete    : reserved # Verify that c{} is concrete.
comparable  : reserved # Verify that instances are comparable.
persistent  : reserved # Verify that instances are persistence-safe.

# Nominal field attributes.
final       : reserved # Can't be overridden in subclasses.
public      : reserved # Field is visible everywhere (the default).
private     : reserved # Field isn't visible outside declaring context.
scoped      : macro    # Field is private to list of scopes or similar.
override    : reserved # Verify that we're overriding a base class field of the same name.
conditional : macro    # Conditional existance.

# Modular overloading.
implicit    : reserved # This field propagates implicitly and specifies the type overloads must adhere to.
implicits   : reserved # Same as the above, plus the top-level symbols contained in overloads become visible with implicit.
overload    : reserved # An overload of a symbol declared implicit elsewhere.

# Declaration attributes anywhere.
shadow      : reserved # TBD. For declaring a variable shadowing an outer scope thing without error.

# Parameter attributes.
optional    : macro    # like ?x:t
named       : macro    # like !x:t if it exists
implicit    : macro    # like %x:t

<# Standard Classes #>

# Guaranteed all instances of the class are unique and comparable by identity.
unique:=class<sealed>.
	Unique<private>^:void

# A standard way of expressing user-visible object lifetimes (unrelated to GC).
deletable:=class<sealed>.
	live<final>[]:logic
	delete<final>():void

########################################################################################################################
# Text functions.

# Basic operations.
utf8(s:string)<decides>:=s where... # If it's a valid utf8 string.
IterateUnicode(s:string)<iterates>:=...

# Human friendly conversion to string.
ToString<implicit>(a:any):string
ToString<overload>(r:rat):string
ToString<overload>(c:char):string
ToString<overload>(s:string):string
ToString<overload>(s:[]unichar):string
ToString<overload>(s:syntax):string

# Canonical conversion from value to syntax.
ToSyntax<implicit>(a:any):syntax
ToVerse<implicit>(a:any):string

########################################################################################################################
# Transactional I/O and top level.

Log(s:stringify)<total>:void
Err(:string)<diverges>:void   # Runtime error.
Refuse(:string)<refuses>:void # Verify error.

########################################################################################################################
# I/O.

# Simple console I/O. Write-only operations are transacts.
Print(s:stringify)<converges><writes>:void
ReadLine()<interacts>:string

# Simple file I/O.
LoadTextFile  (Filename:string               )<interacts><decides>:string
SaveTextFile  (Filename:string,Content:string)<interacts><decides>:void
LoadBinaryFile(Filename:string               )<interacts><decides>:[]nat8
SaveBinaryFile(Filename:string,Content:[]nat8)<interacts><decides>:void

# Start and exit.
GetCommandLine()<interacts><io>:string
GetEnvironment()<interacts><io>:[string]string
Exit(Code:int)<interacts><diverges>:void

# Key I/O.
key_scan_code:type
(OnKeyUp&OnKeyDown)(Callback(Code:key_scan_code):void)<io>:handle

# Legacy I/O.
handle<private>:type
Close(Handle:handle)<io>:void

########################################################################################################################
# First Class Syntax.

# Capturing raw code and hygenic syntax.
syntax            : type
syntax.identifier : type
syntax.atom       : type
syntax.list       : type
