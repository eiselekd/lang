# $RoughId: extconf.rb,v 1.3 2001/08/14 19:54:51 knu Exp $
# $Id: extconf.rb,v 1.3.2.1 2001/08/16 07:35:43 knu Exp $

require "mkmf"

$CFLAGS << " -DHAVE_CONFIG_H -I#{File.dirname(__FILE__)}/.."

$objs = [
  "sha1.#{$OBJEXT}",
  "sha1hl.#{$OBJEXT}",
  "sha1init.#{$OBJEXT}",
]

have_header("sys/cdefs.h")

have_header("inttypes.h")

have_header("unistd.h")

create_makefile("digest/sha1")
