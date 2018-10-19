package templ::utils;
use File::Basename;
use File::Path;

use Data::Dumper;
sub ltrim { my $s = shift; $s =~ s/^\s+//;       return $s };
sub rtrim { my $s = shift; $s =~ s/\s+$//;       return $s };
sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };

sub readfile {
    my ($in) = @_;
    ::usage(\*STDOUT) if (length($in) == 0) ;
    open IN, "$in" or die "Reading \"$in\":".$!;
    local $/ = undef;
    $m = <IN>;
    close IN;
    return $m;
}

sub writefile {
  my ($out,$re,$temp) = @_;
  my $dir = dirname($out);
  if ($dir) {
    mkpath($dir);
  }
    open OUT, ">$out" or die ($out.$!);
    print OUT ($re);
    close OUT;
}

######################################################################
# templating:
#
package templ::template;
use Data::Dumper;
@ISA = ('node');
  
$RE_balanced_squarebrackets = qr'(?:[\[]((?:(?>[^\[\]]+)|(??{$RE_balanced_squarebrackets}))*)[\]])'s;
$RE_balanced_smothbrackets  = qr'(?:[\(]((?:(?>[^\(\)]+)|(??{$RE_balanced_smothbrackets}))*)[\)])'s;
$RE_balanced_brackets =       qr'(?:[\{]((?:(?>[^\{\}]+)|(??{$RE_balanced_brackets}))*)[\}])'s;
$RE_IF =                      qr'\{\{if((?:(?>(?:(?!(?:fi\}\}|\{\{if)).)+)|(??{$RE_IF}))*)fi\}\}'s;
$RE_CALL =                    qr'\{\{call((?:(?>(?:(?!(?:llac\}\}|\{\{call)).)+)|(??{$RE_CALL}))*)llac\}\}'s;
$RE_FILE =                    qr'\{\{file((?:(?>(?:(?!(?:elif\}\}|\{\{file)).)+)|(??{$RE_FILE}))*)elif\}\}'s;
$RE_SLASH =                   qr'\{\{/((?:(?>(?:(?!(?:/\}\}|\{\{/)).)+)|(??{$RE_SLASH}))*)/\}\}'s;

sub snippetParam {
	my ($m) = (shift);
    if ($m =~ /^$RE_balanced_squarebrackets/) {
		$m = substr($m,length($&));
		return ($m,templ::utils::trim($1));
	} else {
		return ($m,"");
	}
}

sub splitpairs {
	my ($m) = @_; my $i, $j; my @a = ();
	for ($j = $i = 0; $i < length($m); $i++) {
		my $p = substr($m,$i);
		if ($p =~ /^\\\s/) {
			$i++;
		} elsif ($p =~ /^(\s+)/) {
			length($&);
			push(@a,[substr($m,$j,$i-$j),$&]);
			$i += length($&);
			$j = $i;
			$i--;
		}
	}
	push(@a,[substr($m,$j,$i-$j),'']) if ($i != $j);
	return [@a];
}

sub resolve_splitpairs {
	my ($a) = (shift);
	return [map { 
		my ($fn,$sp) = ($$_[0],$$_[1]);
		my $_fn = templ::utils::trim($fn);
		$fn = $_fn;
		[$fn,$sp];
	}  @$a ];
}

sub join_splitpairs {
	my ($a) = @_;
	return join("", map { $$_[0].$$_[1] } @$a);
}

sub slashsnippet {
    my ($self,$m) = (shift,shift);
	$m = $self->doSub($m,@_);
	my $a = splitpairs($m,@_); my $i = 0;
	$a = resolve_splitpairs($a,@_);
	$m = join_splitpairs($a,@_);
	return $m;
}

sub filesnippet {
    my ($self,$m) = (shift,shift);
    $m = templ::utils::trim($m);
	$m = templ::node::_relfname($m,@_);
	return $m;
}

sub callsnippet {
    my ($self,$m) = (shift,shift);
	my ($m,$n) = snippetParam($m);
	confess("Cannot find call expression\n") if (!length($n));
    my $a = $self->doSub($m,@_);
	$m = eval($n); warn $@ if $@;
    return $m;
}

sub ifsnippet {
    my ($s,$m) = (shift,shift);
    my ($m,$n) = snippetParam($m);
	confess("Cannot find if expression\n") if (!length($n));
    my $v = $s->getVal($n,@_);
    if (defined($v)) {
		return "" if (!$v);
    } else {
		return "" if ($n=~/^[a-z][a-z0-9_]*$/);
		my $r = eval($n); warn $@ if $@;
		if (!$r) {
			return "";
		}
    }
    $m =~ s/$RE_IF/$self->ifsnippet($1,@_)/gse;
    return $m;
}

sub exesnippet {
    my ($self,$m) = (shift,shift);
    return `$m`;
}

sub flatten {
	my (@v) = @_; my @r = ();
	foreach my $v (@v) {
		if (UNIVERSAL::isa($v,'ARRAY')) {
			push(@r, map { flatten($_) } @$v);
		} else {
			push(@r,$v);
		}
	}
	return @r;
}

sub asrelfname { return $_[0]; }
sub cflags_esc { return $_[0]; }

sub snippet {
    my ($s,$m) = (shift,shift);
    my ($m,$n) = snippetParam($m);
    $v = $$s{$m}  ;
    if (UNIVERSAL::isa($v,'ARRAY')) {
	my $c = exists($$a{'c'}) ? $$a{'c'} : 'txt';
	my @a = map { 
	    (UNIVERSAL::can($_,'doSub') && exists($$_{$c})) ? $_->doSub($$_{$c},@_) : $_ 
	} @{$v};
	my $b = $$a{'join'} || "";
      	@a = map { $pre.$_.$post } @a; 
	$r = join($b,@a);
    } else {
	my $c = exists($$a{'c'}) ? $$a{'c'} : 'txt'; my $cres = undef;
	$_v = (UNIVERSAL::can($v,'doSub') && ($s != $v || $c ne 'txt' ) && defined($cres) ) ? $v->doSub($cres,@_) : $v;
	$r = $pre.$$a{'pre'}.$_v.$$a{'post'}.$post;
    }
    if (exists($$a{'trim'})) {
	$r = join("\n",map { templ::utils::trim($_) } split('[\n]',$r));
	$r = templ::utils::trim($r);
    }
    return $r;
}

sub doSub {
    my ($self,$m) = (shift,shift); my $cnt = 0; my $it = 0;
	my @a = @_;
    while(1) {
		my $ol = length($m);
		$cnt += ($m =~ s/$RE_IF/$self->ifsnippet($1,@a)/gsei);
		$cnt += ($m =~ s/$RE_CALL/$self->callsnippet($1,@a)/gsei);
		$cnt += ($m =~ s/$RE_FILE/$self->filesnippet($1,@a)/gsei);
		$cnt += ($m =~ s/$RE_SLASH/$self->slashsnippet($1,@a)/gsei);
		$cnt += ($m =~ s/\{$RE_balanced_brackets\}/$self->snippet($1,@a)/gse);
		$cnt += ($m =~ s/`([^`]+)`/$self->exesnippet($1,@a)/gsei);
		last if (($ol == length($m)) || $it > 4);
		$it++;
    }
    return $m;
}

sub saveTo {
	my ($s) = (shift);
	my $m = $s->doSub($$s{'txt'},@_);
	if (exists($$s{'trim'})) {
		$m = join("\n",map { templ::utils::trim($_) } split('[\n]',$m));
	}
	my $fn = $s->{'_fname'};
	print("Writing file $fn\n") if ($::OPT{'verbose'} && !$::OPT{'quiet'});
	::utils::writefile($fn,$m);
	print($m) if ($::OPT{'verbose'} && !$::OPT{'quiet'});
	return $m;
}
