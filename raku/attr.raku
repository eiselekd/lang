
role convert {
    method toStruct {
	my %h = self.^attributes.map(
	    -> $i
	    {
		my $v = $i.get_value(self);
		($i.name.substr(2) =>
		 (given $i.type
		  {
		      when Positional   { ( Array.new( $v.map(->$i    { $i.^find_method("toStruct") ?? $i.toStruct !! $i } )) ) }
		      when Associative  { (  Hash.new( $v.map(->$i    { $i.key => ($i.value.^find_method("toStruct") ?? $i.value.toStruct !! $i.value) } )) ) }
		      when Mu           { $v }
		  })
		)
	    }
	);
    }
};

class dada does convert
{
    has $.name;
};


class layout does convert
{
    has $.name;
    has @.lines;
    has %.hash;
};

my $a = layout.new(:name("test"),:lines([1,2,dada.new(:name(3))]),:hash({:a(1),:b(2),:c(3)}));

my $v = $a.toStruct();
say $v;
