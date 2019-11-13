m0:
	python tigress.py tigress-0-challenge-0

prep:
	git clone https://github.com/cea-sec/miasm.git m
	cd m; python3 setup.py install --user
	sudo pip3 install future pyparsing

m1:
	python sandbox_elf_x86_32.py -b reverseMe

prep:
	sudo python install llvmlite


re:
	python recon.py

add:
	gcc -m32 -o add add.c
