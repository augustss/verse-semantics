# A simple nix shell with most of the tools needed in this repository
# In particular since the Haskell programs here are built with Makefiles and not with Cabal
# it is very useful to use ghcWithPackages to get them into the global package set this way


with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    # for the drracket stuf
    racket
    graphviz
    # for the latex stuff
    haskellPackages.lhs2tex
    # for the Haskell stuff
    (ghc.withPackages(p: with p;
      [megaparsec parser-combinators mtl uniplate optparse-applicative smallcheck haskeline
       QuickCheck]))
    ghcid
  ];
}

