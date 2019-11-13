all:

test-keystone:
	python3 asm.py

test-unicorn:
	python3 uni.py

test:
	python3 test.py

prepare-keystone:
	sudo apt-get install cmake
	sudo apt-get build-dep keystone
	rm -rf keystone-src/
	git clone https://github.com/keystone-engine/keystone.git keystone-src; cd keystone-src; git checkout c4de98f71f05f356817f5e91fee0d509c7b0b440; git am ../0001-usr-install.patch
	mkdir -p keystone-src/build keystone-lib/
	cd keystone-src/build; ../make-share.sh
	cp -d keystone-src/build/llvm/lib/x86_64-linux-gnu/libkeystone.so* keystone-lib/
	cd keystone-src/build; sudo make install
	keystone-src/bindings/python; python3 setup.py install --user


prepare-unicorn:
	if [ ! -d unicorn-src ]; then \
		rm -rf unicorn-src; \
		git clone https://github.com/unicorn-engine/unicorn.git unicorn-src; \
		 git checkout 3b17db0d84a2a73deb064e00966edd71338b0321; \
	fi
	cd unicorn-src; UNICORN_ARCHS="arm aarch64 x86 x86_64" ./make.sh;
	cd unicorn-src; sudo make install
	cd unicorn-src/bindings/python; python3 setup.py install --user
