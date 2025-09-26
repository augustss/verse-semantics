# Comments

A code comment explains something about the code, or the programmer's reason for why something is programmed the way it is. When the program runs, code comments are ignored.

```verse
1+2 # Hello
```

single-line comment: Anything that appears between # and the end of line is part of the code comment.

```verse
1<# inline comment #>+2
```

inline block comment: Anything that appears between <# and #> is part of the code comment. Inline block comments can be between expressions on a single line and don’t change the expressions.

```verse
DoThis()
<# And they
can run multiple
long lines #>
DoThat()
```

multi-line block comment: Anything that appears between <# and #> is part of the code comment. Multi-line block comments can span multiple lines.

```verse
<# Block comments nest <# like this #> #>
```

nested block comment: Anything that appears between <# and #> is part of the code comment, and they can nest. This can be useful if you want to comment out some expressions in a line for testing and debugging without changing an existing code comment.

```verse
<#>
    Here is a long
    description spanning
    multiple lines.
 DoThis() # This expression is not part of the indented comment
 ```

indented comment: Anything that appears on new lines after <#> and is indented four spaces over is part of the code comment. The first line that isn’t indented four spaces over is not part of the code comment and ends the code comment.
comments
