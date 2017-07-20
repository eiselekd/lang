all:

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
