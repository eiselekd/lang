--TEST--
Check tcc_define_symbol()
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_define_symbol($tcc, 'SYM', 1);
  $ret = tcc_compile_string($tcc, '
int main()
{
#ifdef SYM
    return 1;
#else
    return 2;
#endif
}
');
  var_dump($ret);
  $ret = tcc_run($tcc);
  var_dump($ret);
?>
--EXPECT--
int(0)
int(1)
