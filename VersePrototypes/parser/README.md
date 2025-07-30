## The Verse Parser

### What

This is the verse parser as a library. It parses source verse concrete syntax to `Language.Verse`.

Clients are expected to write their own translation from `Exp SimpleName` to their own AST.

### Testing

Testing is minimal. The test defined in `test/Main.hs` reads in the verse files
in `VersePrototypes/parser/test_data`, parses them and then pretty prints the
files. By default this displays nothing to `stdout`, to see the output run
`cabal --enable-tests --test-show-details=streaming test verse-parser-test
--test-options='--verbose'` from the `VersePrototypes` directory.

Parser tests live in the `VersePrototypes/` folder. This information is
plumbed as a data directory in cabal to the parser test executable which calls
`getDataDir` to find all `foo.verse` files.
