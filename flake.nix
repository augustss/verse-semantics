{
  description = "A verse flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        py-pkgs = pkgs.python313Packages;
        texEnv = (pkgs.texlive.combine {
          inherit (pkgs.texlive)

          scheme-basic
          hyperref
          xstring
          totpages
          environ
          hyperxmp
          ncctools # for manyfoot
          xkeyval
          microtype
          booktabs
          everyshi
          textcase
          ifmtarg
          acmart
          xcolor
          cmap
          caption
          float
          inconsolata
          cbfonts
          libertine
          txfonts
          comment
          stmaryrd
          polytable
          tikz-cd
          pgf       # contains tikz
          lazylist
          framed
          cleveref
          mathpartir
          conteq
          textgreek
          mathtools
          multirow
          makecell
          upquote
          metafont
          latexmk
          ;
        });

        hPkgs =
          pkgs.haskell.packages."ghc967"; # need to match Stackage LTS version
                                          # from stack.yaml snapshot

        pygments-verse = py-pkgs.buildPythonPackage {
          pname = "pygments-verse";
          version = "0.1.0";
          src = ./Book/libs;
          format = "setuptools";
          doCheck = false;

          # If you want “editable” mode (nixpkgs ≥ 24.05):
          editable = false;

          propagatedBuildInputs = with py-pkgs; [
          mkdocs mkdocs-material pygments pymdown-extensions
          ];
        };

        verse-lexer = py-pkgs.buildPythonPackage {
          pname = "verse-lexer";
          version = "1.0.0";
          src = ./Book;
          format = "setuptools";
          doCheck = false;

          # If you want “editable” mode (nixpkgs ≥ 24.05):
          editable = false;

          propagatedBuildInputs = with py-pkgs; [
          mkdocs mkdocs-material pygments pymdown-extensions
          pygments-verse
          ];
        };


        devTools = [
          hPkgs.ghc                     # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid                   # Continuous terminal Haskell compile checker
          hPkgs.hlint                   # Haskell codestyle checker
          hPkgs.hoogle                  # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie            # auto generate LSP hie.yaml file from cabal
          hPkgs.cabal-install
          hPkgs.lhs2tex                 # for latex make
          hPkgs.fast-tags               # for TAGS files
          hPkgs.eventlog2html
          texEnv                        # the latex packages
          stack-wrapped
          pkgs.ghostscript              # for ps2pdf
          pkgs.zlib # External C library needed by some Haskell packages

          ## python deps for verse book
          pkgs.python313
          verse-lexer
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix:         # We don't want Stack's way of integrating Nix.
        # --system-ghc      # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
          shellHook = ''
          '';
        };
      });
}