## QualIdent
x;
(y:)x

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
