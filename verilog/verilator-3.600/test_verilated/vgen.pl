#!/usr/bin/perl -w
#$Id$
######################################################################
#
# This program is Copyright 2001-2006 by Wilson Snyder.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of either the GNU General Public License or the
# Perl Artistic License.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
######################################################################

require 5.006_001;
use Getopt::Long;
use IO::File;
use Pod::Usage;
use Data::Dumper; $Data::Dumper::Indent = 1;
use Bit::Vector;
use strict;
use vars qw ($Debug);

our @Orig_ARGV = @ARGV;
our $Rerun_Args = $0." ".join(' ',@Orig_ARGV);

use vars qw (@Blocks
	     %Vars
	     %VarsBlock
	     %Tree
	     @Commit
	     $Depth
	     %IdWidth
	     %Ops);

#======================================================================

# width=>	Number of bits the output size is, 0=you tell me. 
# func=>	What to put in output file
# signed=>	0=unsigned output, 1=signed output, '%1'=signed if op1 signed
# em=>		How to calculate emulated return value
#	%w	Width of this output op ($treeref->{width})
#	%v	Output value ($treeref->{val})
#	%1r	First operand ($treeref->{op1})
#	%1v	First operand value ($treeref->{op1}{val})
#	%1w	First operand width ($treeref->{op1}{width})

our $Raise_Weight_Max = 50;
%Ops =
(
 'VCONST'=>	{weight=>1&&20, width=>0, 	    sc=>1, terminal=>1, v=>'%v', },
 'VIDNEW'=>	{weight=>1&&10, width=>0, 	    sc=>1, terminal=>0, v=>'%i', },
 'VIDOLD'=>	{weight=>1&&20, width=>0, 	    sc=>1, terminal=>0, v=>'%i', },
 'VRANGE'=>	{weight=>1&&30, width=>0, signed=>0,sc=>0, terminal=>0, v=>'%i[%2:%3]', },
 'VBITSEL'=>	{weight=>1&&10, width=>1, signed=>0,sc=>0, terminal=>0, v=>'%i[%2]', },
 'VBITSELP'=>	{weight=>1&&10, width=>0, signed=>0,sc=>0, terminal=>0, v=>'%i[%2+:%3]', },
 'VBITSELM'=>	{weight=>1&&10, width=>0, signed=>0,sc=>0, terminal=>0, v=>'%i[%2-:%3]', },
 # Unary
 'VEXTEND'=>	{weight=>1&&3, width=>-2, signed=>0,sc=>0, terminal=>0, v=>'{%xw\'h0,%1}', },
 'VLOGNOT'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(! %1)', },
 'VREDAND'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(& %1)', },
 'VREDOR'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(| %1)', },
 'VREDNAND'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(~& %1)', },
 'VREDNOR'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(~| %1)', },
 'VREDXNOR'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(^~ %1)', },
 'VREDXOR'=>	{weight=>1&&1, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(^ %1)', },
 'VNOT'=>	{weight=>1&&3, width=>0, 	    sc=>1, terminal=>0, v=>'(~ %1)', },
 'VUNARYMIN'=>	{weight=>1&&2, width=>0, 	    sc=>1, terminal=>0, v=>'(- %1)', },
 'VCOUNTONES'=>	{weight=>0&&2, width=>32, signed=>0, sc=>0, terminal=>0, v=>'\$countones(%1)', },  # No ncv support
 'VONEHOT'=>	{weight=>0&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'\$onehot(%1)', },  # No ncv support
 'VONEHOT0'=>	{weight=>0&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'\$onehot0(%1)', },  # No ncv support
 # Binary
 'VAND'=>	{weight=>1&&2, width=>0, 	    sc=>1, terminal=>0, v=>'(%1 & %2)', },
 'VOR'=>	{weight=>1&&2, width=>0, 	    sc=>1, terminal=>0, v=>'(%1 | %2)', },
 'VNAND'=>	{weight=>1&&0, width=>0, 	    sc=>0, terminal=>0, v=>'(%1 ~& %2)', },  #FIX vcs bug!
 'VNOR'=>	{weight=>1&&0, width=>0, 	    sc=>0, terminal=>0, v=>'(%1 ~| %2)', },  #FIX vcs bug!
 'VXOR'=>	{weight=>1&&2, width=>0, 	    sc=>1, terminal=>0, v=>'(%1 ^ %2)', },
 'VXNOR'=>	{weight=>1&&0, width=>0, 	    sc=>0, terminal=>0, v=>'(%1 ^~ %2)', },  #FIX vcs bug!
 'VEQ'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 == %2)', },
 'VNEQ'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 != %2)', },
 'VGT'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 >  %2)', },
 'VGTE'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 >= %2)', },
 'VLT'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 <  %2)', },
 'VLTE'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 <= %2)', },
 'VEQCASE'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 === %2)', }, # FIX just a = for now
 'VNEQCASE'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 !== %2)', }, # FIX just a != for now
 'VLOGOR'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 || %2)', },
 'VLOGAND'=>	{weight=>1&&2, width=>1, signed=>0, sc=>0, terminal=>0, v=>'(%1 && %2)', },
 'VADD'=>	{weight=>1&&10, width=>0, 	    sc=>1, terminal=>0, v=>'(%1 + %2)', },
 'VSUB'=>	{weight=>1&&10, width=>0, 	    sc=>1, terminal=>0, v=>'(%1 - %2)', },
 'VMUL'=>	{weight=>1&&15,width=>0, 	    sc=>1, terminal=>0, v=>'(%1 * %2)', },  # High % as rarely applyable
 #'VDIV'=>	{weight=>2&&0, width=>-32,	    sc=>1, terminal=>0, v=>'(%1 / %2)', }, # FIX
 #'VMODDIV'=>	{weight=>2&&0, width=>-32,	    sc=>1, terminal=>0, v=>'(%1 %% %2)', }, # FIX
 #'VPOW'=>	{weight=>2&&0,width=>-64, 	    sc=>0, terminal=>0, v=>'(%1 ** %2)', },
 'VSHIFTL'=>	{weight=>1&&8, width=>0, signed=>0, sc=>0, terminal=>0, v=>'(%1 << %2)', },
 'VSHIFTLS'=>	{weight=>1&&8, width=>0, signed=>1, sc=>0, terminal=>0, v=>'(%1 <<< %2)', },
 'VSHIFTR'=>	{weight=>1&&8, width=>0, signed=>0, sc=>0, terminal=>0, v=>'(%1 >> %2)', },
 'VSHIFTRS'=>	{weight=>1&&15,width=>0, signed=>1, sc=>0, terminal=>0, v=>'(%1 >>> %2)', }, # ShiftR seems to sign extend differently for <=32 and >32 bits
 'VCONCAT'=>	{weight=>1&&4, width=>-2,signed=>0, sc=>0, terminal=>0, v=>'{%1,%2}', },
 'VREPLIC'=>	{weight=>1&&2, width=>0, signed=>0, sc=>0, terminal=>0, v=>'{%1{%2}}', },
 'VREPLIC1W'=>	{weight=>1&&2, width=>0, signed=>0, sc=>0, terminal=>0, v=>'{%1{%2}}', },
 'VSIGNED'=>	{weight=>1&&2, width=>0, signed=>1, sc=>0, terminal=>0, v=>'\$signed(%1)', },
 'VUNSIGNED'=>	{weight=>1&&2, width=>0, signed=>0, sc=>0, terminal=>0, v=>'\$unsigned(%1)', },
 # Triops
 'VCOND'=>	{weight=>1&&4, width=>0,            sc=>0, terminal=>0, v=>'(%1 ? %2 : %3)', },
 # Control flow
  #VIF
  #VFOR
  #VCASE
  #VCASEX
  #VCASEZ
 );

my %ops2 =
(
 'VCONST'=>	{pl=>'', 			rnd=>'rnd_const(%tr);'},
 'VIDNEW'=>	{pl=>'%tv=$Vars{%i}{val};',
		 rnd=>'%i=next_id(%tw); $Vars{%i}=gen_leaf(width=>%tw,trunc=>1,signed=>%tg); id_commit(%tr,"%i");1;',},
 'VIDOLD'=>	{pl=>'%tv=$Vars{%i}{val};',	rnd=>'%i=old_id(%tr);',   ok_id_width=>1,},
 'VRANGE'=>	{pl=>'VRANGE(%tr,$Vars{%i}{val},%2v,%3v);',	rnd=>'%i=next_id(%tw); my $lsb=rnd(128-%tw); my $msb=$lsb+%tw-1;  %2r=val_leaf($msb); %3r=val_leaf($lsb); $Vars{%i}=gen_leaf(width=>($msb+1));'},
 'VBITSEL'=>	{pl=>'VRANGE(%tr,$Vars{%i}{val},%2v,%2v);',	rnd=>'%i=next_id(%tw); my $wid=min(128,rnd_width()|3); %2r=gen_leaf(width=>(log2($wid)-1),signed=>0); $Vars{%i}=gen_leaf(width=>$wid);'},
 'VBITSELP'=>	{pl=>'VBITSELP(%tr,$Vars{%i}{val},%2v,%3v);',	rnd=>'%i=next_id(%tw); my $wid=min(128,(%tw+rnd_width()|3)); %3r=val_leaf(%tw); my $maxval = $wid-%tw; %2r=(($maxval<4)?val_leaf($maxval):gen_leaf(width=>(log2($maxval)-1),signed=>0)); $Vars{%i}=gen_leaf(width=>$wid);'},
 'VBITSELM'=>	{pl=>'VBITSELM(%tr,$Vars{%i}{val},%2v,%3v);',	rnd=>'%i=next_id(%tw); my $wid=min(128,(%tw+rnd_width()|3)); %3r=val_leaf(%tw); my $maxval = $wid-1; my $minval=%tw-1; %2r=val_leaf(rnd($maxval-$minval)+$minval); $Vars{%i}=gen_leaf(width=>$wid);'},  # No easy way to make expr with specified minimum
 # Unary
 'VEXTEND'=>	{pl=>'VRESIZE  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>rnd_width(%tw-1));'},
 'VLOGNOT'=>	{pl=>'VLOGNOT  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDAND'=>	{pl=>'VREDAND  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDOR'=>	{pl=>'VREDOR   (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDNAND'=>	{pl=>'VREDNAND (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDNOR'=>	{pl=>'VREDNOR  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDXOR'=>	{pl=>'VREDXOR  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VREDXNOR'=>	{pl=>'VREDXNOR (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VNOT'=>	{pl=>'VNOT     (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VUNARYMIN'=>	{pl=>'VUNARYMIN(%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VCOUNTONES'=>	{pl=>'VCOUNTONES(%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VONEHOT'=>	{pl=>'VONEHOT  (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 'VONEHOT0'=>	{pl=>'VONEHOT0 (%tr,%1v);',	rnd=>'%1r=gen_leaf(width=>0);'},
 # Binary
 'VAND'=>	{pl=>'VAND   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VOR'=>	{pl=>'VOR    (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VNAND'=>	{pl=>'VNAND   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VNOR'=>	{pl=>'VNOR    (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VXOR'=>	{pl=>'VXOR   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VXNOR'=>	{pl=>'VXNOR  (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 'VEQ'=>	{pl=>'VEQ    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VNEQ'=>	{pl=>'VNE    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VGT'=>	{pl=>'VGT    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VGTE'=>	{pl=>'VGE    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VLT'=>	{pl=>'VLT    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VLTE'=>	{pl=>'VLE    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VEQCASE'=>	{pl=>'VEQ    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VNEQCASE'=>	{pl=>'VNE    (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>%1w);'},
 'VLOGOR'=>	{pl=>'VLOGOR (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>0);'},
 'VLOGAND'=>	{pl=>'VLOGAND(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>0);   %2r=gen_leaf(width=>0);'},
 'VADD'=>	{pl=>'VADD   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);', trunc=>1,},
 'VSUB'=>	{pl=>'VSUB   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);', trunc=>1,},
 'VMUL'=>	{pl=>'VMUL   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);', trunc=>1,},  # Multiply generates larger width, so need truncate for safety
 #'VDIV'=>	{pl=>'VDIV   (%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 #'VMODDIV'=>	{pl=>'VMODDIV(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>%tw,signed=>%tg);'},
 #'VPOW'=>	{pl=>'VPOW   (%tr,%1r,%2r);',	rnd=>'%1r=gen_leaf(width=>min(%tw,6),signed=>%tg); %2r=gen_leaf(width=>min(%tw,8),signed=>%tg);', trunc=>1,},  # Generates larger width, so need truncate for safety
 'VSHIFTL'=>	{pl=>'VSHIFTL(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>log2(%tw)+1,signed=>%tg);'},
 'VSHIFTLS'=>	{pl=>'VSHIFTL(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>log2(%tw)+1,signed=>%tg);'},
 'VSHIFTR'=>	{pl=>'VSHIFTR(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>log2(%tw)+1,signed=>%tg);'},
 'VSHIFTRS'=>	{pl=>'VSHIFTRS(%tr,%1v,%2v);',	rnd=>'%1r=gen_leaf(width=>%tw,signed=>%tg); %2r=gen_leaf(width=>log2(%tw)+1,signed=>%tg);'},
 'VCONCAT'=>	{pl=>'VCONCAT(%tr,%1v,%2v);',	rnd=>'my $d=(rnd(%tw-2)+1); %1r=gen_leaf(width=>$d); %2r=gen_leaf(width=>(%tw-$d));'},
 'VREPLIC'=>	{pl=>'VREPLIC(%tr,%1v,%2v);',	rnd=>'my $d=rnd_rep_width(%tw); %1r=val_leaf($d); %2r=gen_leaf(width=>(%tw/$d));'},
 'VREPLIC1W'=>	{pl=>'VREPLIC(%tr,%1v,%2v);',	rnd=>'%1r=val_leaf(%tw); %2r=gen_leaf(width=>1);'},
 'VSIGNED'=>	{pl=>'VCLONE (%tr,%1v,0);',	rnd=>'%1r=gen_leaf(width=>%tw);'},
 'VUNSIGNED'=>	{pl=>'VCLONE (%tr,%1v,0);',	rnd=>'%1r=gen_leaf(width=>%tw);'},
 # Triops
 'VCOND'=>	{pl=>'VCOND(%tr,%1v,%2v,%3v);',	rnd=>'%1r=gen_leaf(width=>1); %2r=gen_leaf(width=>%tw,signed=>%tg); %3r=gen_leaf(width=>%tw,signed=>%tg);'},
 );

foreach my $op (keys %ops2) {
    while ((my $key,my $val) = each %{$ops2{$op}}) {
	$Ops{$op}{$key} = $val;
    }
}

#======================================================================
# main

#Bit::Vector->Configuration("ops=arithmetic");


my  $opt_seed=5;
our $Opt_NumOps = 300;
our $Opt_Depth = 4;
our $Opt_Signed = 1;
our $Opt_Raise;
our $Opt_Sc;
our $Opt_BlockStmts = 2;
our $Signed_Pct = 60;
$Debug = 0;
if (! GetOptions (
		  "help"	=> \&usage,
		  "debug"	=> \&debug,
		  "depth=i"	=> \$Opt_Depth,
		  "blockstmts=i"=> \$Opt_BlockStmts,
		  "numops=i"	=> \$Opt_NumOps,
		  "raise=i"	=> \$Opt_Raise,
		  "seed=i"	=> \$opt_seed,
		  "signed!"	=> \$Opt_Signed,
		  "sc!"		=> \$Opt_Sc,
		  "<>"		=> \&parameter,
		  )) {
    usage();
}

if ($opt_seed==0) {
    srand();
    $opt_seed = rnd(1<<20)+1;
    $Rerun_Args =~ s/-seed[= ]+0/-seed=$opt_seed/;
    print "  $Rerun_Args\n";
}
srand($opt_seed);
init();
gentest();
write_output_sc("vgen.cpp") if $Opt_Sc;
write_output_v("vgen.v") if !$Opt_Sc;

#----------------------------------------------------------------------

sub usage {
    print '$Id$ ', "\n";
    pod2usage(-verbose=>2, -exitval => 2);
    exit (1);
}

sub debug {
    $Debug = 1;
}

sub parameter {
    my $param = shift;
    die "%Error: Unknown parameter: $param\n";
}
 
#######################################################################
#######################################################################
#######################################################################
#######################################################################
# Global Functions

sub init {
    for my $op (keys %Ops) {
	my $opref = $Ops{$op};
	$opref->{name} = $op;
	gen_v($opref);
	gen_pl($opref);
	gen_rnd($opref);
	$opref->{weight} = 0 if $Opt_Sc && !$opref->{sc};
	$opref->{weight} = 2 if $Opt_Sc && $op eq 'VCONST';
    }
    raise();
}

sub raise {
    for (my $i=0; $i<($Opt_Raise||0); $i++) {
	my @ops = (values %Ops);
	while (1) {
	    my $rndop = $ops[rnd($#ops + 1)];
	    next if !$rndop->{weight};  # Don't turn on disabled ops
	    $rndop->{weight} += rnd($Raise_Weight_Max);
	    printf "\tWeight %-15s +%d\n",$rndop->{name},$rndop->{weight};
	    last;
	}
    }
}

sub gentest {
    for (my $opn=0; $opn<$Opt_NumOps/$Opt_BlockStmts; $opn++) {
	do_a_test();
    }
}

#######################################################################
# Randomization

sub _rnd_op_ok {
    my $opref = shift;
    my $paramref = shift;
    return (($opref->{width} == 0
	     || $opref->{width} == $paramref->{width}
	     || ($opref->{width}==-32 && $paramref->{width}<=32)     # -32... must be <32 bits
	     || ($opref->{width}==-64 && $paramref->{width}<=64)     # -64... must be <64 bits
	     || ($opref->{width}==-2  && $paramref->{width}>=2)     # -2... must be >2 bits
	     )
	    && (!$opref->{ok_id_width} || $IdWidth{$paramref->{width}}{$paramref->{signed}||0})
	    && (!defined $opref->{signed} || ($opref->{signed} == ($paramref->{signed}||0)))
	    && (!$opref->{trunc} || $paramref->{trunc})
	    && (!$opref->{opt_signed} || $Opt_Signed)
	    && (($Depth < $Opt_Depth && !$paramref->{need_terminal})
		|| $opref->{terminal}));
}

sub rnd_op {
    my $paramref = shift;

    my $totweight = 0;
    foreach my $opref (values %Ops) {
	if (_rnd_op_ok($opref,$paramref)) {
	    $totweight += $opref->{weight};
	}
    }
    my $chooseweight = rnd($totweight);
    $totweight = 0;
    foreach my $opref (values %Ops) {
	if (_rnd_op_ok($opref,$paramref)) {
	    $totweight += $opref->{weight};
	    if ($chooseweight < $totweight) {
		return $opref;
	    }
	}
    }
    die "%Error: No instructions match,";
}

sub rnd_width {
    my $max = shift;
    my $v = rnd(100);
    my $n = (0
	     || (($v<20) &&  1)
	     || (($v<25) &&  2)
	     || (($v<30) && 31)
	     || (($v<35) && 32)
	     || (($v<40) && 63)
	     || (($v<45) && 64)
	     || (($v<50) && 95)
	     || (($v<55) && 96)
	     || (rnd(128)+1));
    if ($Opt_Sc) {
	$n = (0
	      #|| (($v<50) && 32)
	      || (32));
	#(!$max) or die "%Error: --sc max must 32/64/96,";
    }
    if ($max && $n>=$max) { $n = rnd($max-1)+1; }
    return $n;
}

sub rnd_rep_width {
    my $out = shift;
    return 1 if $out==1;
    # We'd like to pick any divisor that works.
    my @factors;
    for (my $div=1; $div<$out; $div++) {
	if (int($out/$div)==($out/$div)) {
	    push @factors, $div;
	}
    }
    my $fac = $factors[rnd($#factors+1)];
    #print "RND REP $out -> $fac (@factors)\n" if $Debug;
    return $fac;
}

sub rnd_const {
    my $treeref = shift;
    my $width = $treeref->{width} or die;
    my $v = rnd(100);

    my $val = Bit::Vector->new($width);
    if ($v<25) {	# zero
    } elsif ($v<50) {	# ones
	$val->Word_Store(0,~0);
	$val->Word_Store(1,~0) if $width>32;
	$val->Word_Store(2,~0) if $width>64;
	$val->Word_Store(3,~0) if $width>96;
    } elsif ($v<60) {	# one
	$val->Word_Store(0,1);
    } else { #random
	$val->Word_Store(0,rnd_int());
	$val->Word_Store(1,rnd_int()) if $width>32;
	$val->Word_Store(2,rnd_int()) if $width>64;
	$val->Word_Store(3,rnd_int()) if $width>96;
    }
    $treeref->{val} = $val;
}

sub rnd_int {
    my $v = rnd(100);
    return 0 if ($v<25);
    return ~0 if ($v<50);
    return 1 if ($v<60);
    return rnd32();
}
  
sub rnd {
    return (int(rand($_[0]))) if ($_[0] < (1<<15));
    return (rnd32() % $_[0]);
}
sub rnd32 {
    my $vp = int(rand(1<<16));
    $vp ^= (int(rand(1<<8)))<<16;# Single 1<<16 doesn't work
    $vp ^= (int(rand(1<<8)))<<24;
    return ($vp);
}

#######################################################################

our $Next_Id = 0;
sub next_id {
    # Note width hasn't been determined yet
    $Next_Id++;
    my $id = sprintf("W%04d",$Next_Id);
    return $id;
}
sub id_commit {
    my $treeref = shift;
    my $width = $treeref->{width};
    my $signed = $treeref->{signed};
    my $id = shift;
    push @Commit, sub {
	$IdWidth{$width}{$signed} = [] if !$IdWidth{$width}{$signed};
	push @{$IdWidth{$width}{$signed}}, $id;
	$VarsBlock{$id}{set} = 1;
	1;
    };
}

sub old_id {
    my $treeref = shift;
    my $width = $treeref->{width};
    my $signed = $treeref->{signed};

    my $n = $#{$IdWidth{$width}{$signed}} + 1;
    my $idn = rnd($n);
    my $id = $IdWidth{$width}{$signed}[$idn];
    $VarsBlock{$id}{used} = 1;
    return $id;
}

sub write_output_v {
    my $filename = shift;

    my $fh = IO::File->new($filename, "w") or die("%Error: $! $filename,\n");
    print $fh "// Created by: $Rerun_Args\n";

    print $fh "module vgen (clk, check, done);\n";
    print $fh "   input clk;\n";
    print $fh "   input check;\n";
    print $fh "   output done;\n";
    print $fh '   initial $write("\n*** Vgen.v starting, seed = ',$opt_seed,'\n");',"\n";

    print $fh "   // verilator lint_off UNSIGNED\n";
    print $fh "   // verilator lint_off CMPCONST\n";
    print $fh "   // verilator lint_off WIDTH\n";
    print $fh "\n";

    my $cycles = 2;

    foreach my $var (sort (keys %Vars)) {
	print $fh "",decl_text ($var),"\n";
    }

    foreach my $block (@Blocks) {
	print $fh "\t//".('='x60)."\n";
	my $style = rnd(100);
	if ($style < 15) {
	    # This allows statements to get split up, and constants to propagate
	    print $fh "   always @(", join(" or ", ('check', @{$block->{inputs}}));
	    print $fh ") begin : $block->{name}\n";
	    print $fh @{$block->{preass}};
	    print $fh "   end\n";
	    print $fh "   always @(posedge clk) begin : $block->{name}Check\n";
	    print $fh @{$block->{body}};
	    print $fh "   end\n";
	}
	elsif ($style < 40) {
	    print $fh "   always @(", join(" or ", ('check', @{$block->{inputs}}));
	    print $fh ") begin : $block->{name}\n";
	    print $fh @{$block->{preass}};
	    print $fh @{$block->{body}};
	    print $fh "   end\n";
	    }
	else {
	    foreach my $stmt (@{$block->{preass}}) {
		$cycles++;
		print $fh "   always @(posedge clk) begin\n";
		$stmt =~ s/ = / <= /mg;
		print $fh $stmt;
		print $fh "   end\n";
	    }
	    print $fh "   always @(posedge clk) begin\n";
	    print $fh @{$block->{body}};
	    print $fh "   end\n";
	}
    }

    print $fh  "\n";
    print $fh "   reg done; initial done=1'b0;\n";
    print $fh "   reg ddone; initial ddone=1'b0;\n";
    print $fh "   always @(posedge clk) begin\n";
    print $fh "      if (check) begin\n";
    print $fh "         done <= 1'b1;\n";
    print $fh "      end\n";
    print $fh "      if (done && !ddone) begin\n";
    print $fh "         ddone <= 1'b1;\n";
    print $fh '         $write("*-* All Finished *-*\n");',"\n";
    print $fh "      end\n";
    print $fh "   end\n";

    print $fh "\n";
    print $fh "   parameter [31:0] CYCLES /*verilator public*/ = $cycles;\n";
    print $fh "endmodule\n";

    $fh->close();
}

sub write_output_sc {
    my $filename = shift;

    my $fh = IO::File->new($filename, "w") or die("%Error: $! $filename,\n");

    print $fh "// -*- SystemC -*-\n";
    print $fh "// Created by: $Rerun_Args\n";

    # Classes
    foreach my $block (@Blocks) {
	print $fh "class $block->{name};\n";
    }
    print $fh "\n";

    # Headers
    print $fh "//".('='x60)."\n";
    print $fh "SC_MODULE(Vgen) {\n";
    print $fh "public:\n";
    print $fh "  sc_in_clk clk;\n";
    print $fh "  sc_in<bool> check;\n";
    print $fh "  static const int CYCLES /*verilator public*/ = ",$#Blocks+3,";\n";
    print $fh "\n";
    foreach my $var (sort (keys %Vars)) {
	print $fh "",decl_text ($var,"sc_signal"),"\n";
    }
    print $fh "\n";
    foreach my $block (@Blocks) {
	print $fh "  $block->{name}*  ".lc($block->{name}).";\n";
    }
    print $fh "  SC_CTOR(Vgen);\n";
    print $fh "};\n\n";

    # Sub Interface
    print $fh "//".('='x60)."\n";
    foreach my $block (@Blocks) {
	print $fh "SC_MODULE($block->{name}) {\n";
	print $fh "public:\n";
	print $fh "  sc_in_clk clk;\n";
	print $fh "  sc_in<bool> check;\n";
	foreach my $var (@{$block->{inputs}}) {
	    print $fh "",decl_text ($var,"sc_in"),"\n";
	}
	foreach my $var (@{$block->{outputs}}) {
	    print $fh "",decl_text ($var,"sc_out"),"\n";
	}
	print $fh "  SC_CTOR($block->{name});\n";
	print $fh "  void clkPosedge();\n";
	print $fh "};\n\n";
    }

    # Implementation
    print $fh "//".('='x60)."\n";
    print $fh "SP_CTOR_IMP(Vgen) : clk(\"clk\"), check(\"check\") {\n";
    foreach my $block (@Blocks) {
	print $fh "  //\n";
	print $fh "  SP_CELL (".lc($block->{name}).", $block->{name});\n";
	print $fh "   SP_PIN (".lc($block->{name}).", clk, clk);\n";
	print $fh "   SP_PIN (".lc($block->{name}).", check, check);\n";
	foreach my $var (@{$block->{inputs}}, @{$block->{outputs}}, ) {
	    print $fh "   SP_PIN (".lc($block->{name}).", $var, $var);\n";
	}
    }
    print $fh "}\n\n";

    # Sub Implementations
    print $fh "//".('='x60)."\n";
    foreach my $block (@Blocks) {
	print $fh "SP_CTOR_IMP($block->{name}) {\n";
	print $fh "  SC_METHOD(clkPosedge);\n";
	print $fh "  sensitive_pos(clk);\n";
	print $fh "}\n\n";

	print $fh "void $block->{name}::clkPosedge() {\n";
	print $fh @{$block->{preass}};
	print $fh @{$block->{body}};
	print $fh "}\n\n";
    }

    $fh->close();
}

######################################################################

sub callers {
    for (my $i=0; ; $i++) {
	my @c = caller($i);
	last if !$c[0];
	print "Caller $i: ",join(' ',@c[0..3]),"\n";
    }
}

#######################################################################
#######################################################################
#######################################################################
#######################################################################
# Code generation/emitting Functions

sub do_a_test {
    local $Depth = 0;
    @Commit = ();
    %VarsBlock = ();

    my $block = {
	name=>"Block".($#Blocks+2),
	body=>[],
	preass=>[],
	inputs=>[],
	outputs=>[],
    };

    for (my $i=0; $i<$Opt_BlockStmts; $i++) {
	my $treeref = gen_leaf(width=>0);
	push @{$block->{body}},
	"\tif ($treeref->{text} != ".$treeref->val_to_text().") if (check) ".stop_text().";\n";
    }

    foreach my $var (keys %VarsBlock) {
	push @{$block->{inputs}}, $var
	    if $VarsBlock{$var}{used} && !$VarsBlock{$var}{set};
    }

    foreach my $var (reverse (sort (keys %Vars))) {
	my $varref = $Vars{$var};
	next if $varref->{printedit};
	$varref->{printedit} = 1;
	push @{$block->{outputs}}, $var;
	push @{$block->{preass}}, sprintf ("\t$var = %s;\n"
					 ,$varref->{text});
    }

    foreach my $com (@Commit) {
	&{$com} or die "%Error: Can't eval:\n$com\n $@ ";
    }

    push @Blocks, $block;
}

sub gen_leaf {
    my $inforef = {width=>0,  # Anything
		   need_terminal=>0,
		   #trunc=>undef,	# Allow multiply op
		   @_};

    $inforef->{width} ||= rnd_width();
    $inforef->{signed} = ($Opt_Signed && $inforef->{width}>1 && (rnd(100)<$Signed_Pct))?1:0
	if !defined $inforef->{signed};
    print +(("  "x$Depth)."Leaf of width $inforef->{width}\n") if $Debug;
    my $op = rnd_op($inforef);

    my $treeref = new Vg::Base;
    while ((my $key,my $val) = each %{$op}) {
	$treeref->{$key} = $val;
    }
    while ((my $key,my $val) = each %{$inforef}) {
	$treeref->{$key} = $val;
    }

    local $Depth = $Depth+1;
    print "RndSub $treeref->{rnd_sub_text}\n" if $Debug;
    $treeref->{rnd_sub}($treeref);
    $treeref->tree_dump() if $Debug;

    print "RndPl\n" if $Debug;
    $treeref->{pl_sub}($treeref);
    print "RndV\n" if $Debug;
    $treeref->{text} = $treeref->{v_sub}($treeref);
    print "Done\n" if $Debug;
    print "  Value ",$treeref->{val}," = ",$treeref->val_to_text(),"\n" if $Debug;
    #$treeref->tree_dump() if $Debug;

    $treeref->{val_size} = $treeref->{val}->Size;   #Debugging
    $treeref->{val_text} = $treeref->{val}->to_Hex; #Debugging

    ($treeref->{val}->Size == $treeref->{width}) or die "%Error: Size mismatch,";

    return $treeref;
}

sub gen_v {
    my $opref = shift;

    my $fmt = $opref->{v};
    $fmt =~ s/%1/%s/g;
    $fmt =~ s/%2/%s/g;
    $fmt =~ s/%3/%s/g;
    $fmt =~ s/%v/%s/g;
    $fmt =~ s/%i/%s/g;
    $fmt =~ s/%xw/%s/g;

    my $argl = $opref->{v};
    my @args;
    while ($argl =~ s/(%xw|%.)//) {
	my $arg = $1;
	push @args, '$treeref->{op1}{text}'	if $arg =~ /%1/;
	push @args, '$treeref->{op2}{text}'	if $arg =~ /%2/;
	push @args, '$treeref->{op3}{text}'	if $arg =~ /%3/;
	push @args, '$treeref->val_to_text'	if $arg =~ /%v/;
	push @args, '$treeref->{id}'		if $arg =~ /%i/;
	push @args, '$treeref->{width}-$treeref->{op1}{width}'		if $arg =~ /%xw/;
    }

    my $func = ("sub { "
		." my \$treeref = shift;"
		." sprintf(\"$fmt\",".join(',',@args).");"
		."}");
    my $set = ("\$opref->{v_sub} = $func; 1;");
    $opref->{v_sub_text} = $func;	# For seeing it in debugging dumps
    #print "Op V  $opref->{name} $set\n";
    eval($set) or die "%Error: Can't eval:\n$set\n $@ ";
}

sub escapes {
    my $str = shift;
    my $cmt = shift;
    $str =~ s/%tr/\$treeref/g;
    $str =~ s/%tg/\$treeref->{signed}/g;
    $str =~ s/%tv/\$treeref->{val}/g;
    $str =~ s/%tw/\$treeref->{width}/g;
    #
    $str =~ s/%1r/\$treeref->{op1}/g;
    $str =~ s/%1n/(\$treeref->{op1}{val}->Word_Read(0))/g;
    $str =~ s/%1v/\$treeref->{op1}{val}/g;
    $str =~ s/%1w/\$treeref->{op1}{width}/g;
    #
    $str =~ s/%2r/\$treeref->{op2}/g;
    $str =~ s/%2n/(\$treeref->{op2}{val}->Word_Read(0))/g;
    $str =~ s/%2v/\$treeref->{op2}{val}/g;
    $str =~ s/%2w/\$treeref->{op2}{width}/g;
    #
    $str =~ s/%3r/\$treeref->{op3}/g;
    $str =~ s/%3n/(\$treeref->{op3}{val}->Word_Read(0))/g;
    $str =~ s/%3v/\$treeref->{op3}{val}/g;
    $str =~ s/%3w/\$treeref->{op3}{width}/g;
    #
    $str =~ s/%i/\$treeref->{id}/g;
    ($str !~ /%/) or die "%Error: $cmt: Unknown %% escape in $str,";
    return $str;
}

sub gen_pl {
    my $opref = shift;

    my $str = escapes($opref->{pl}, $opref->{name});
    my $func = ("sub { "
		." my \$treeref = shift;"
		." $str;"
		."}");
    my $set = ("\$opref->{pl_sub} = $func; 1;");
    $opref->{pl_sub_text} = $func;	# For seeing it in debugging dumps
    #print "Op PL $opref->{name} $set\n";
    eval($set) or die "%Error: Can't eval:\n$set\n $@ ";
}

sub gen_rnd {
    my $opref = shift;

    my $str = escapes($opref->{rnd}, $opref->{name});

    my $func = ("sub { "
		." my \$treeref = shift;"
		." $str;"
		."}");
    my $set = ("\$opref->{rnd_sub} = $func; 1;");
    $opref->{rnd_sub_text} = $func;	# For seeing it in debugging dumps
    #print "Op RND $opref->{name} $set\n";
    eval($set) or die "%Error: Can't eval:\n$set\n $@ ";
}

sub stop_text {
    return $Opt_Sc?'USTOP()':'$stop';
}

sub decl_text {
    my $var = shift;
    my $decl_with = shift;

    my $varref = $Vars{$var};
    if ($Opt_Sc) {
	(!$varref->{signed}) or die "%Error: No signed SystemC yet\n";
	my $type = ((   ($varref->{val}->Size == 32) && "uint32_t")
		    || (($varref->{val}->Size == 64) && "uint64_t"));
	$type or die "%Error: Unknown Size ".$varref->{val}->Size,",";
	return sprintf "  %s<%s> %s; //=%s"
	    , $decl_with, $type, $var, $varref->{val}->to_Hex;
    } else {
	return sprintf "   reg %s [%3d:0] %s %s; //=%s"
	    , ($varref->{signed}?"signed":"      ")
	    , ($varref->{val}->Size)-1,
	    , $var
	    , (rnd(100)<30 ? "/*verilator public*/":(" "x length("/*verilator public*/")))
	    , $varref->{val}->to_Hex;
    }
}

#######################################################################
#######################################################################
#######################################################################
#######################################################################
# Math Functions

sub val_leaf { return {width=>32, signed=>0, val=>Bit::Vector->new_Dec(32,$_[0]), text=>$_[0],}; }

sub makebool { return (Bit::Vector->new_Dec(1,$_[0])); }
sub newsized { return (Bit::Vector->new($_[0]->Size)); }
sub max { return $_[0]<$_[1] ? $_[1] : $_[0]; }
sub min { return $_[0]>$_[1] ? $_[1] : $_[0]; }

sub log2 {
    for (my $i=31; $i>=0; $i--) {
	return $i+1 if $_[0]>(1<<$i);
    }
    return 0;
}

sub countones {
    my $out = 0;
    for (my $bit=0; $bit < $_[0]->Size; $bit++) {
	$out ++ if $_[0]->bit_test($bit);
    }
    return $out;
}


sub VLOGNOT   { $_[0]{val} = makebool(($_[1]->is_empty)?1:0); }
sub VUNARYMIN { $_[0]{val} = my $o = newsized($_[1]); $o->Negate($_[1]); }
sub VCOUNTONES { $_[0]{val} = Bit::Vector->new_Dec(32,countones($_[1])); }
sub VONEHOT  { $_[0]{val} = makebool((countones($_[1])==1)?1:0); }
sub VONEHOT0 { $_[0]{val} = makebool((countones($_[1])<=1)?1:0); }
sub VLOGAND { $_[0]{val} = makebool((!($_[1]->is_empty) && !($_[2]->is_empty))?1:0); }
sub VLOGOR  { $_[0]{val} = makebool((!($_[1]->is_empty) || !($_[2]->is_empty))?1:0); }

sub VCOND   { if (!($_[1]->is_empty)) { $_[0]{val}=$_[2]->Clone; } else { $_[0]{val}=$_[3]->Clone; } }

sub VREDAND { $_[0]{val} = makebool(($_[1]->is_full)?1:0); }
sub VREDOR  { $_[0]{val} = makebool(($_[1]->is_empty)?0:1); }
sub VREDNAND { $_[0]{val} = makebool(($_[1]->is_full)?0:1); }
sub VREDNOR  { $_[0]{val} = makebool(($_[1]->is_empty)?1:0); }
sub VREDXOR {
    my $out = 0;
    for (my $bit=0; $bit < $_[1]->Size; $bit++) {
	$out ^= $_[1]->bit_test($bit);
    }
    $_[0]{val} = makebool($out);
}
sub VREDXNOR {
    my $out = 1;
    for (my $bit=0; $bit < $_[1]->Size; $bit++) {
	$out ^= $_[1]->bit_test($bit);
    }
    $_[0]{val} = makebool($out);
}
sub eithercompare { ($_[1]->{signed} && $_[2]->{signed})
			? $_[1]{val}->Compare($_[2]{val})
			: $_[1]{val}->Lexicompare($_[2]{val}); }
sub VEQ { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])==0) ?1:0); }
sub VNE { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])!=0) ?1:0); }
sub VLT { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])< 0) ?1:0); }
sub VLE { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])<=0) ?1:0); }
sub VGT { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])> 0) ?1:0); }
sub VGE { $_[0]{val} = makebool( (eithercompare($_[0],$_[1],$_[2])>=0) ?1:0); }

sub VSHIFTLxx {
    print "$Vars{vq}->ShiftL($_[0],$_[1]);\n";
    print "    ",$_[0]->to_Hex,"  ",$_[1]->to_Hex,";\n";
    my $out = $_[0]->Clone;
    $out->Move_Left($_[1]->Word_Read(0));
    print $out->to_Hex,"\n";
    return $out; }
sub VAND { $_[0]{val}=my $o=newsized($_[1]); $o->Intersection($_[1],$_[2]); }
sub VOR  { $_[0]{val}=my $o=newsized($_[1]); $o->Union($_[1],$_[2]); }
sub VNAND { $_[0]{val}=my $o=newsized($_[1]); $o->Intersection($_[1],$_[2]); $o->Complement($o); }
sub VNOR  { $_[0]{val}=my $o=newsized($_[1]); $o->Union($_[1],$_[2]); $o->Complement($o); }
sub VXOR { $_[0]{val}=my $o=newsized($_[1]); $o->ExclusiveOr($_[1],$_[2]); }
sub VXNOR{ $_[0]{val}=my $o=newsized($_[1]); $o->ExclusiveOr($_[1],$_[2]); $o->Complement($o); }
sub VNOT { $_[0]{val}=my $o=newsized($_[1]); $o->Complement($_[1]); }
sub VSHIFTL{ $_[0]{val}=my $o=$_[1]->Clone; $o->Move_Left ($_[2]->Word_Read(0)); }
sub VSHIFTR{ $_[0]{val}=my $o=$_[1]->Clone; $o->Move_Right($_[2]->Word_Read(0)); }
sub VSHIFTRS{$_[0]{val}=my $o=$_[1]->Clone; $o->Move_Right($_[2]->Word_Read(0));
	     if ($_[1]->msb() && $_[2]->Word_Read(0)>0) {$o->Interval_Fill(max(0,$o->Size-1-$_[2]->Word_Read(0)), $o->Size-1); }
	     #print (" SHI ",$_[0]{val}->to_Hex,' = ',$_[1]->to_Hex,' >>> ',$_[2]->Word_Read(0),"\n");
}
sub VCLONE { $_[0]{val}=$_[1]->Clone; }
sub VRESIZE {
    $_[0]{val}=$_[1]->Clone;
    $_[0]{val}->Resize($_[0]{width});
}
sub VADD { $_[0]{val}=my $o=newsized($_[1]); $o->add($_[1],$_[2],0); }
sub VSUB { $_[0]{val}=my $o=newsized($_[1]); $o->subtract($_[1],$_[2],0); }
sub VMUL { # Multiply is signed, so need an additional sign bit
	   my $a=$_[1]->Clone; $a->Resize($_[1]->Size + 1);
	   my $b=$_[2]->Clone; $b->Resize($_[1]->Size + 1);
	   my $mo=Bit::Vector->new($_[1]->Size + $_[2]->Size + 1);
	   $mo->Multiply($a,$b);
	   my $o=newsized($_[1]); $o->Interval_Copy($mo,0,0,$_[1]->Size);
	   $_[0]{val}=$o;
	   }
sub VDIV { my $a=$_[1]->Clone; my $b=$_[2]->Clone;  # Else it will take them as signed #s
	   $a->Resize($a->Size + 1); $b->Resize($b->Size + 1);
	   print ("//DIVpp ",$_[1]->to_Hex,' ',$_[2]->to_Hex,' ',$_[1]->Size,'.',$_[2]->Size," \n");
	   print ("//DIVpp ",$a->to_Hex,' ',$b->to_Hex,' ',$a->Size,'.',$b->Size," \n");
	   my $o=newsized($a); my $rem=newsized($a);
	   if (!$_[2]->is_empty) { $o->Divide($a,$b,$rem); } # No division by zero
	   #push @Lines, ("//DIV ",$_[1]{val}->to_Hex,' ',$_[2]->to_Hex,' ',$o->to_Hex,'.',$rem->to_Hex," \n");
	   $_[0]{val}=$o; }
sub VPOW { # Power is a signed operation
    my $a=$_[1]{val}->Clone; if (!$_[1]->{Signed}) { $a->Resize($_[1]{val}->Size + 1); }
    my $b=$_[2]{val}->Clone; if (!$_[2]->{Signed}) { $b->Resize($_[2]{val}->Size + 1); }
    print "VVpow = ",$_[1]{val}->to_Hex," ** ",$_[2]{val}->to_Hex,"\n";
    my $mo=Bit::Vector->new($_[1]{val}->Size + 1);
    $mo->Power($a,$b);
    my $o=Bit::Vector->new($_[0]{width}); $o->Interval_Copy($mo,0,0,$_[1]{val}->Size);
    $_[0]{val}=$o;
    print "VV = $o\n";
}
sub VRANGE { #print "RANGE ",$_[1]->to_Hex,' ',$_[2]->to_Hex,' ',$_[3]->to_Hex," \n";
	   return VRANGE_CONST($_[0],$_[1],$_[2]->Word_Read(0),$_[3]->Word_Read(0)); }
sub VBITSELP {
	   return VRANGE_CONST($_[0],$_[1],$_[2]->Word_Read(0)+$_[3]->Word_Read(0)-1, $_[2]->Word_Read(0)); }
sub VBITSELM {
	   return VRANGE_CONST($_[0],$_[1],$_[2]->Word_Read(0), $_[2]->Word_Read(0)-$_[3]->Word_Read(0)+1); }
sub VRANGE_CONST { #print "RANGE ",$_[1]->to_Hex,' ',$_[2],' ',$_[3]," \n";
             my $size = $_[2] - $_[3] + 1;
             my $o=Bit::Vector->new($size);
	     if ($_[3] < $_[1]->Size) {
		 $o->Interval_Copy($_[1],0,$_[3],$size);
	     }
	     $_[0]{val}=$o; }
sub VCONCAT { my $o=Bit::Vector->new($_[1]->Size + $_[2]->Size);
	      $o->Interval_Copy($_[1],$_[2]->Size,0,$_[1]->Size);
	      $o->Interval_Copy($_[2],0,0,$_[2]->Size);
	      $_[0]{val}=$o; }
sub VREPLIC { my $o=Bit::Vector->new($_[1]->Word_Read(0) * $_[2]->Size);
	      my $pos = 0;
	      for (my $time=0; $time<($_[1]->Word_Read(0)); $time++) {
		  $o->Interval_Copy($_[2],$pos,0,$_[2]->Size);
		  $pos += $_[2]->Size;
	      }
	      $_[0]{val}=$o; }

#######################################################################
#######################################################################
#######################################################################

package Vg::Base;
use Data::Dumper;
use strict;

#------------------------------------------------------------
# CREATORS

sub new {
    my $class = shift;
    my $self = {
	width=>0,	# Width of expression, 0=Pick a width
	#signed=>0/1,	# Undef = pick a sign
	@_};
    bless $self, $class;
    return $self;
}

# ACCESSORS

#------------------------------------------------------------
# OUTPUTTING

sub val_to_text {
    my $treeref = shift;
    my $val = lc $treeref->{val}->to_Hex();
    $val = "0" if $treeref->{val}->is_empty;
    if ($Opt_Sc) {
	return ("0x".$val
		.(($treeref->{width}>32)?"ULL":"UL")
		);
    } else {
	return ($treeref->{width}
		.($treeref->{signed}?"'sh":"'h")
		.$val);
    }
}

sub tree_dump {
    my $treeref = shift;
    print Dumper($treeref);
}
  
#######################################################################
__END__

=pod

=head1 NAME

vgen.pl - Generate random verilog code

=head1 SYNOPSIS

  vgen.pl

=head1 DESCRIPTION

vgen.pl generates automatic random verilog programs.

=head1 ARGUMENTS

=over 4

=item --help

Displays this message and program version and exits.

=item --blockstmts

Number of statements per block.  Defaults to 2.

=item --depth

Maximum depth of generated expressions.

=item --initial

Put all statements into an initial block.  This will probably be optimized
down to a NOP.

=item --numops

Number of operations to create. 

=item --raise

Pick the specified number of random opcodes, and raise their frequency.

=item --sc

Output SystemC code (Experimental).

=item --seed

Seed for the random number generator.  Defaults to 5, 0=randomize.

=item --signed

Include some signed arithmetic in the generated code.  Experimental.

=back

=head1 SEE ALSO

=head1 AUTHORS

Wilson Snyder <wsnyder@wsnyder.org>

=cut

######################################################################
### Local Variables:
### compile-command: "./vgen.pl -sc --depth=10 --blockstmts=10"
### compile-command: "make "
### End:
