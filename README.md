# Verse

## Syntactic form

Syntax | Comment
------ | -------
`p:=e`   | Binding
`f[a]` | Function call that may fail
`f(a)` | Function call that must not fail
`a\|b` | Alternative values
`:any` | Alternatives containing all values
`if(c) then t else e` | 
`for(c) e` |
`e1,e2,...,en` | tuple/array
`p => e` | lambda expression
... | ...

## Primitive functions

Name | Meaning
---- | -------
`x+y` | add x and y
`x<y` | yield x or fail
`x=y` | unify, yield x or fail
`false` | function from empty set to empty set
`int`| identity on integers, otherwise fails
`any` | identity function
`type` | 

## Macros

These syntactic construct are just sugar.

Syntax | Expansion
------ | ---------
`p:e` | `p := :e`
`:e` | `e[:any]`
