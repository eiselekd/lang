all:
	vala v.vala
	valac v.vala -o v.exe
	valac -C v.vala -o v.c

e:
	valac --pkg gtk+-3.0 e.vala -o e.exe

prepare:
	apt-get install libgtk-3-dev
