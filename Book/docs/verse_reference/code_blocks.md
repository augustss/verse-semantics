# Code Blocks

A code block is a group of expressions, and introduces a new scope for variables and constants.

A code block, or block, is a group of zero or more expressions that introduces a new scoped body. (A block with
zero expressions would be an empty block, and ideally would only be used as a placeholder to be filled in later.)

Code blocks can only appear after identifiers.

Scope refers to the part of the program where the association of an identifier (name) to a value is valid, and where that name can be used to refer to the value. For example, any constants or variables that you create
within a code block only exist in the context of the code block. This means that the lifetime of objects is
limited to the scope they're created in and they cannot be used outside of that code block.

The following example shows how to calculate the maximum number of arrows that can be bought with the number of coins the player has. The constant MaxArrowsYouCanBuy is created within the if block and therefore its scope is limited to the if block. When the constant MaxArrowsYouCanBuy is used in the print string, it produces an error because the name MaxArrowsYouCanBuy doesn't exist in the scope outside of the if expression.

```verse
CoinsPerQuiver : int = 100
ArrowsPerQuiver : int = 15
var Coins : int = 225
if (MaxQuiversYouCanBuy : int = Floor(Coins / CoinsPerQuiver)):
    MaxArrowsYouCanBuy : int = MaxQuiversYouCanBuy * ArrowsPerQuiver
Print("You can buy at most {MaxArrowsYouCanBuy} arrows with your coins.") # Error: Unknown identifier MaxArrowsYouCanBuy
```

Verse doesn't support reusing an identifier even if it's declared in a different scope, unless you can qualify the identifier by adding (qualifying_scope:) before the identifier, where qualifying_scope is the name of an identifier's module, class, or interface. Whenever you define and use the identifier, you must also add a qualifier to the identifier.

For more details, see module, class, and interface.

Code Block Formats

Code blocks have three possible formats in Verse. They are all semantically equivalent, so you can change the style of a code block without changing what it does.
If you nest a code block inside of another code block, you must still use an identifier at the beginning of the nested code block. To nest code, use the block expression.

Spaced Format

With this format, the block begins with :, with each expression that follows on its own line. Each line is uniformly indented four spaces.

```verse
if (test-arg-block):
    expression1
    expression2
```

Note that if (test-arg-block) is not part of the block, but the block starts at the end of that line with :.
You can also use ; to separate multiple expressions on a single line.
Multi-Line Braced Format
The block is enclosed by {}, and expressions are on new lines.

```verse
if (test-arg-block)
{
    expression1
    expression2
}
```

You can also use ; to separate multiple expressions on a single line.

Single-Line Dot Format

With this format, the block begins with . with each expression on the same line, and each expression is separated by ; instead of being placed on a new line.

```verse
if (test-arg-block). expression1; expression2
```

If you use the single-line dot format in an if expression that has an else, then you can only have one expression before the else. For example:

```verse
if (test-arg-block). expression1 else. expression2
```
