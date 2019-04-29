


all:
	export PYTHONPATH=$$PYTHONPATH:$(CURDIR)/nmigen; \
	python3 rec.py generate > rec.v ; cat rec.v

test:
	export PYTHONPATH=$$PYTHONPATH:$(CURDIR)/nmigen; \
	python3 nmigen/examples/alu_hier.py generate

prepare:
	pip3 install bitarray pyvcd
