export NODE=$(CURDIR)/tools/node-linux-x64/bin/node
export LLVM_ROOT=$(CURDIR)/tools/install/bin
export BINARYEN=$(LLVM_ROOT)

all:

h:
		export NODE=$(CURDIR)/tools/node-linux-x64/bin/node; \
		export LLVM=$(CURDIR)/tools/install/bin; \
		export BINARYEN=$(CURDIR)/tools/install; \
	$(CURDIR)/tools/install/emscripten/emcc h.c -o h.html
	@echo "http://localhost:8000/h.html"
	pnpm dev



c:
		export NODE=$(CURDIR)/tools/node-linux-x64/bin/node; \
		export LLVM=$(CURDIR)/tools/install/bin; \
		export BINARYEN=$(CURDIR)/tools/install; \
	$(CURDIR)/tools/install/emscripten/emcc c.cpp \
	-s RESERVED_FUNCTION_POINTERS=20 \
	-s USE_PTHREADS=1 \
	-s PTHREAD_POOL_SIZE=5 \
	-s PROXY_TO_PTHREAD=1 \
	-s TOTAL_MEMORY=1073741824 \
	--bind \
	-o c.html
	#
	@echo "http://localhost:8000/c.html"
	pnpm dev


dep:
	mkdir -p tools
	if [ ! -f emscripten.tgz ]; then \
		wget https://storage.googleapis.com/perfetto/emscripten-2.0.12-linux.tgz -O emscripten.tgz; \
	fi
	if [ ! -f node.tgz ]; then \
		wget https://storage.googleapis.com/chromium-nodejs/20.11.0/f9a337cfa0e2b92d3e5c671c26b454bd8e99769e -O node.tgz; \
	fi
	tar -oxf emscripten.tgz -C tools 
	tar -oxf node.tgz -C tools

dep-emscripten:
	-rm $(CURDIR)/tools/install//emscripten/.emscripten
	#
	NODE=$(CURDIR)/tools/node-linux-x64/bin/node \
	LLVM_ROOT=$(CURDIR)/tools/install/bin/node \
	$(CURDIR)/tools/install/emscripten/emcc c.cpp
