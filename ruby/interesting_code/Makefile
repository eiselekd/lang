
prep:
	if [ ! -d metasm ]; then git clone https://github.com/jjyg/metasm.git metasm; fi

f.exe:
	gcc -g f.c -o f.exe

m: prep
	ruby m.py

p: prep f.exe
	ruby p.py > p.txt

.PHONY: f.exe
