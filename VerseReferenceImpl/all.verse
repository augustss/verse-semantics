var X:int = 1
prefix'[]'(t:type)(Xs:any) := for (X:Xs). t[X]

a<b>. c where d
a => b where c

set a|b += 1;

set Object.Variable += 1;



all . 1 +2 where 3
all. set Z = F(:Xs, Z)



f():int = return;
f()<x>:int = return;
f3(o:O0)<transacts><decides>:int = (return o?)
for(X:=1..2 where 3){4}







Cons := function (Y:int, Z:int). all{Y | (:Z)}
all{Y | (:Z)}


X->Y := 1..3
enum1 := enum<persistable><public>:
      A


A.ParametricSubobject.Field <>  B.ParametricSubobject.Field

xxx(X:C){1}

xxxx:



x<public>:int
x<public>():int
class<internal>{}
class<internal>(xxx){x<public>:int}
class<internal>(xxx):
   x<public>:int

X = :C



set pat /= :C
X := :C
X or (:C)



f<native>() : void


map[1] = 1
map{""=>0}[""]=0
map{""=>0}("")=0
map{""=>0}{""}=0




assert{A := c0{}; B := c0{}; A.ParametricSubobject.Field<>B.ParametricSubobject.Field}


assert:
    var A:[]int=array{}
    B:=if (true?):
        set A += array{0}




assert{Concatenate(for(I:=0..100) do array{})=array{}};

InsertAt(Array:[N]t, Index:int, Value:t where N:int, t:type, 0<=Index<=N):[N+1]t=1;





Func()<decides>:int = 1

ClassA:=xxxx<abstract>:

my_class<public> := class<internal>(xxx){x<public>:int}

my_class<public> := class<internal>{x<public>:int}

class2{}.F();

var X:int = 0

f():int = return;
f():int = return 0=1;
F(X:Curry(operator'+')(Y) where Y:int = 1) := X
MFun<protected>(X:int)<transacts>:float = X*10+1
MFun<public><protected>(X:int)<transacts><suspends>:float = X*10+1


X(Y:int):(Z:Int):int ;
X(Y:int):(Z:Int):(W:int):int ;
F(0);

X where Y

X < Y < Z
X > Y > Z
X > Y < Z
# X < Y > Z not ok


## QualIdent
x;
(y:)x

## Specs  NOT YET
x<a>;
x<a><b>;
#a with <a>;

## If
if(0){0}else x;
if(0){0};
if(0)then 1 else x;
if(0)then 1;
if(0)then {1} else x;
if(0)then {1};
if(0)then 1 else {2};
if(0)then 1 else. 2;
if(0)then 1 else if (2) then 3;
if(0). 1;
if(0). 1 else 2;
if{fail};
## Base
(0);
0;
### Char
'a';
'\n';
'\'';
0o21;
0u00022;
#/apa;    Can't recognise this path
(/path:)apa
### String
"a";
"abc";
"";
"a\nb\\c";
"a{2}b";
"a{"c"}b";
#Markup
if(0). 1;
x;
0

## Postfix
f(0){1};
f(0)do{2; 3};
f(0)do. 4;
f(0)do 5;
f{1};
f{1}do{2};
f{1}until{2};
f{1}until 2;
f{1}catch(2){3};
f(0);
g(1){1}catch(2){3};
f at {1};
f at 1;
f of {1};
f of 1;
x^;
x?;
x[1];
x.y;
x^[1](2);
## Prefix
^x;
?x;
[1]x;
+x;
-x;
*x;
-^x;
#+{1;2};
## Mul
x*y;
x/y;
#x&y;
x*y*z;
## Add
x+y;
x-y;
x-y-z;
## To
#x to y;
x..y;
x->y;
x->y->z;
## Choose
x|y;
x|y|z;
## Greater
x>y;
x>=y;
x>y>z;
## Less
x<y;
x<=y;
x<y<z;
## NotEq
x<>y;
x<>y<>z;
## Eq
x=y;
x=y=z;
## Not
not x;
not not x;
## And
x and y;
x and y and z;
## Or
x or y;
x or y or z;
## Def 1
## In
#in x;
:x;
### Var
#var x;
#set x;
#ref x;
#alias x;
var x = 1;
var x := 1;
set x += 1;
set x *= 1;
set x /= 1;
set x = {1};
set x = {1; 2};
var x =
  1
  2;
set x =
  1
  2;
var x :=
  1
  2;
set x +=
  1
  2;
###
x:=y;
x:=y:=z;
x:={y};
#x where {y};
#x where y;
#x where y,z;
#x is {y};
#x is y;
## Def 2
#..x;
#&x;
## Def 3
return;
yield;
break;
continue;
#return x;
#return {x;y};
## Fun
#x over {y};
#x when {y};
#x while {y};
#x over y;
#x over y, z;
x => y;
x => {y};
#x next y;
## Expr
#x @f();
#x @f() @g();
#@f() x;
#@f() @g() x;
## Commas
1,2,3;
##
## various :, =
x:int;
x:int=1;
x:int:=1;
(x:int)=1;
## Possible ambiguity
x:y<z>;
## Markup
# <M; {x}\n<#x#>foo>;   NOTYET
#<M; a<B;b>>;
#<M>foo</M>;
#<M,N;a>;
#<M(){};a>;
#<M(){}(a){b};a>;
#<M.N; a>;
#<M(0).N; a>;
#<M; ~a ~b ~c d>;
#<M:>
# a b
# c
#<M;>
#<M;a
#b>;
