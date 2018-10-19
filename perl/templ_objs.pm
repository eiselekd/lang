package templ::obj1;
@ISA = ('templ::template');

$ptxt=<<'PEOF';
<?xml version="1.0" encoding="UTF-8"?>
<projectDescription>
  <name>{{id}}</name>
  <comment></comment>
  <projects>
    {{objar}}	
  </projects>
</projectDescription>
PEOF

sub new {
    my ($c,$a) = @_;
    my $name = "name_obj2";
    my $s = {'_id'=>$name,'_name'=>$name,'txt'=>$ptxt,'objar'=>$a};
    bless $s,$c;
    return $s;
}


package templ::obj2;
@ISA = ('templ::template');

$ptxt=<<'PEOF';
<obj2>
  <name>{{id}}</name>
  <comment></comment>
</obj2>
PEOF


sub new {
    my ($c) = @_;
    my $name = "name_obj2";
    my $s = {'_id'=>$name,'_name'=>$name,'txt'=>$ptxt};
    bless $s,$c;
    return $s;
}
