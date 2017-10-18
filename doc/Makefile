all:
	cd sphinx; make html
	echo "julia v0.7 or later required"; cd julia-documenter; make 

prep:
	sudo pip install sphinx
	sudo pip install sphinx_rtd_theme


julia:
	git clone https://github.com/JuliaLang/julia.git
	cd julia; make
