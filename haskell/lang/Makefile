all:

0_sort:
	ghc 0_sort.hs; ./0_sort

1_newtyp:
	ghc 1_newtyp.hs; ./1_newtyp

2_print:
	ghc 2_print.hs; ./2_print

cabal:
	cabal update	
	cabal sandbox init
	cabal install --only-dependencies
	cabal install happy
	cabal install hoogle
	cabal install happy
	cabal install ghc-mod
	cabal configure
	cabal repl
