#!/bin/sh
#
# $RoughId: test.sh,v 1.5 2001/07/13 15:38:27 knu Exp $
# $Id: test.sh,v 1.1.2.1 2001/08/16 07:35:42 knu Exp $

RUBY=${RUBY:=ruby}
MAKE=${MAKE:=make}
CFLAGS=${CFLAGS:=-Wall}

${RUBY} extconf.rb --with-cflags="${CFLAGS}"
${MAKE} clean
${MAKE}

mkdir -p lib/digest

for algo in md5 rmd160 sha1 sha2; do
    (cd $algo &&
	${RUBY} extconf.rb --with-cflags="${CFLAGS}";
	${MAKE} clean;
	${MAKE})
    ln -sf ../../$algo/$algo.so lib/digest/
done

${RUBY} -I. -I./lib test.rb 

rm lib/digest/*.so
rmdir lib/digest
