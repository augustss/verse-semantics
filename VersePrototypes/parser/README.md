## The Verse Parser

### What

This is the verse parser as-a-library. It parses source verse concrete syntax to `Parser.Expr`.

Clients are expected to write their own translation from `Parser.Expr` to their own AST.

### Testing

Parser tests live in the `VersePrototypes/versetest` folder. This information is
plumbed as a data directory in cabal to the parser test executable which calls
`getDataDir` to find all `foo.verse` files.
