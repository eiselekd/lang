#	export PATH=/home/eiselekd/bin-cmake-3.4.3:$$PATH;
# -DZIG_FORCE_EXTERNAL_LLD=1

all: compile

#CXX=clang-6.0  CXXFLAGS="-std=c++11 -stdlib=libstdc++"
configure:
	rm -rf build; mkdir -p build
	cd build; \
	CXX=clang-6.0 cmake  ..
	#
	make compile

compile:
	cd build; \
		VERBOSE=1 make -j 1

apt:
	apt-get install llvm-6.0-dev
