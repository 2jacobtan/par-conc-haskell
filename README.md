# par-conc-haskell

using ghcup, set ghc-8.10.7  \
(for compatibility with `base < 4.15` required by parconc-examples \
https://www.haskell.org/ghcup/

in `./parconc-examples/cabal.project.local` use  \
`with-compiler: ghc-8.6.5` for compatibility with accelerate library  \
otherwise `with-compiler: ghc-8.10.7` is fine.

`cabal build` inside ./parconc-examples  \
(ignore outdated instructions in the repo)

---

./parconc-examples here was originally obtained via  \
`git clone https://github.com/simonmar/parconc-examples/commit/a6c89f015584a2cc2f3b01356b0d6ed5cd0f0a67`
