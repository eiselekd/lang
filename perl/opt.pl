#!/usr/bin/perl
use Getopt::Long;

Getopt::Long::Configure(qw(bundling));
GetOptions(\%OPT,qw{
    quiet|q+
    verbose|v+
    pdir|d=s
    makefile|m=s
    builddir|b=s
    root=s@
    os=s@
    eclipse-internal=i
    dbgtrans
    dbggraph
    dbggen
    dbgparse
    dbgval
} ,@g_more) or usage(\*STDERR);

print ($OPT{'pdir'});
