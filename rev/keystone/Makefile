all:
	python3 asm.py

prepare:
	sudo apt-get install cmake
	sudo apt-get build-dep keystone
	rm -rf keystone-src/
	git clone https://github.com/keystone-engine/keystone.git keystone-src; cd keystone-src; git checkout c4de98f71f05f356817f5e91fee0d509c7b0b440; git am ../0001-usr-install.patch
	mkdir -p keystone-src/build keystone-lib/
	cd keystone-src/build; ../make-share.sh
	cp -d keystone-src/build/llvm/lib/x86_64-linux-gnu/libkeystone.so* keystone-lib/
	keystone-src/bindings/python; python3 setup.py install --user
