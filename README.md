# ghc-syb & ghc-syb-utils

This repo provides [SYB][syb] instances for GHC < 7.0 and SYB utils
for GHC >= 7.10.

[syb]: https://hackage.haskell.org/package/syb

NOTE: GHC 8.0 or later is *not* supported by this package. The GHC AST
has changed dramatically and functionionality provided by `showData`
are now provided via GHC's `HsDumpAst` module.

