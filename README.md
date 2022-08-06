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

---

To build `distrib-db`, you gotta:

add to `cabal.project.local`:

```
source-repository-package
  type: git
  -- Normally this is a git URL, but a local path works too (but I think it must be absolute?):
  location: https://github.com/haskell-distributed/distributed-process/
  -- Replace this with the commit you want to check out:
  tag: cacb9d9ad84a5bdc6c6e6d4e8c7ff1835c89dce9
  
```