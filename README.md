# Verse

## Primitives

Syntax | Comment
------ | -------
`p:=e`   | Binding
`f[a]` | Function call that may fail
`f(a)` | Function call that must not fail
`a\|b` | Alternative values
`:any` | Alternatives containing all values
... | ...

## Macros

These syntactic construct are just sugar.

Syntax | Expansion
------ | ---------
`p:e` | `p := :e`
`:e` | `e[:any]`
