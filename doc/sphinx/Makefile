
all-html:
	sphinx-build -b html src/ out/html

all-latex:
	sphinx-build -b latex src/ out/latex
	cd out/latex; make all-pdf
	chromium-browser out/latex/*pdf

all-text:
	sphinx-build -b text src/ out/text
	sphinx-build -b man src/ out/man
	sphinx-build -b texinfo src/ out/texinfo
	cd out/texinfo; make all

prep:
	sudo pip install sphinx
	sudo pip install sphinx_rtd_theme

