all:

prepare:
	sudo apt-get install nodejs mongodb-server
	npm install


start:
        nodejs b0.js

test:
        ./node_modules/mocha/bin/mocha

test-dbg:
        ./node_modules/mocha/bin/mocha --inspect --debug-brk
