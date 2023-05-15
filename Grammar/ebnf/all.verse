## QualIdent
x;
(y:)x

## Specs
x<a>;
x<a><b>;
a with <a>;

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

## Base
(0);
0;
### Char
'a';
'\n';
'\'';
0o21;
0u00022;
/apa;
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
f(0)do{1};
f(0)do 1;
f{1};
f{1}do{2};
f{1}until{2};
f{1}until 2;
f{1}catch(2){3};
f(0);
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
+{1;2};
## Mul
x*y;
x/y;
x&y;
x*y*z;
## Add
x+y;
x-y;
x-y-z;
## To
x to y;
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
### In
in x;
:x;
### Var
var x;
set x;
ref x;
alias x;
var x = 1;
var x := 1;
var x += 1;
var x *= 1;
var x /= 1;
var x = {1};
###
x:=y;
x:=y:=z;
x:={y};
x where {y};
x where y;
x where y,z;
x is {y};
x is y;
## Def 2
..x;
&x;
## Def 3
return;
yield;
break;
continue;
return x;
return {x;y};
## Fun
x over {y};
x when {y};
x while {y};
x over y;
x over y, z;
x => y;
x => {y};
x next y;
## Expr
x @f();
x @f() @g();
@f() x;
@f() @g() x;
## Commas
1,2,3;
##
## various :, =
x:int;
x:int=1;
x:int:=1;
(x:int)=1;
## Markup
<M; {x}\n<#x#>foo>;
<M; a<B;b>>;
<M>foo</M>;
<M,N;a>;
<M(){};a>;
<M(){}(a){b};a>;
<M.N; a>;
<M(0).N; a>;
<M; ~a ~b ~c d>;
<M:>
 a b
 c
<M;>
<M;a
b>
