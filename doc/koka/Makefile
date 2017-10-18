# note: start "out/debug/koka-0.8.0-dev -ilib" once to load std lib

compile: # <v0.8.0> version:
	cd koka; jake compiler variant=debug
	cd koka; jake compiler variant=release

compile-0.6: # <v0.6.0-dev> version:
	cd koka-0.6; jake compiler variant=debug
	cd koka-0.6; jake compiler variant=release
	cd madoko-0.6; jake madoko

spec: # make shure the --html option is removed (see "prepare" rule)
	cd koka; jake spec variant=release

doc: # --html --htmlcss=styles/madoko.css;styles/koka.css
	 cd koka; out/release/koka-0.8.0-dev -c -l --outdir=out/spec -idoc/spec  -ilib -itest/algeff -itest/lib kokaspec.kk.md spec.kk.md getstarted.kk.md overview.kk.md
	 cd koka; out/release/koka-0.8.0-dev -c -l --outdir=out/spec -idoc/spec  -ilib -itest/algeff -itest/lib tok.kk

prepare:
	if [ ! -d koka-0.6 ]; then git clone https://github.com/koka-lang/koka.git koka-0.6; cd koka-0.6; git checkout v0.6.0-dev ; fi
	if [ ! -d koka ]; then git clone https://github.com/koka-lang/koka.git; cd koka; fi
	sed -i'' 's/" --html " + docflags/" "/g' koka/Jakefile.js
	if [ ! -d madoko-0.6 ]; then git clone https://github.com/koka-lang/madoko.git madoko-0.6; fi
	if [ ! -f madoko ]; then ln -s madoko-0.6 madoko; fi

ubuntu:
	sudo apt install ghc nodejs cabal-install
	cabal update
	sudo npm install -g jake
	sudo npm install -g madoko
	cd koka-0.6; sudo npm install
	cd koka; sudo npm install
	cd madoko-0.6; jake config

de:
	cp koka-0.6.tar.lzma tmp-koka-0.6.tar.lzma; lzma -d tmp-koka-0.6.tar.lzma; tar xvf tmp-koka-0.6.tar
	cp koka-0.8.tar.lzma tmp-koka-0.8.tar.lzma; lzma -d tmp-koka-0.8.tar.lzma; tar xvf tmp-koka-0.8.tar

ubuntu-xelatex:
	sudo apt install texlive-xetex
	sudo apt install texlive-science

test:
	cd madoko-precompiled; npm install
	node madoko-precompiled/lib/cli.js test.md
	node madoko-precompiled/lib/cli.js --pdf test.md

