--TEST--
Check tcc_compile_string
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_compile_string($tcc, '
    int main(){php_printf("Hello World.\n"); return 0;}
  ');
  var_dump($ret);
  $ret = tcc_run($tcc);
  var_dump($ret);
?>
--EXPECT--
int(0)
Hello World.
int(0)
