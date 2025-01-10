--TEST--
Check tcc_compile_string
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_OBJ);
  $ret = tcc_output_file($tcc, 'hello.o');
  var_dump($ret);
  $ret = tcc_compile_string($tcc, '
    int main(){php_printf("Hello World.\n"); return 0;}
  ');
  var_dump($ret);
  $ret = tcc_output_file($tcc, 'hello.o');
  var_dump($ret);
  unset($tcc);
  $tcc = tcc_new();
  $ret = tcc_add_file($tcc, 'hello.o');
  var_dump($ret);
  $ret = tcc_run($tcc);
  var_dump($ret);
?>
--EXPECT--
int(0)
int(0)
int(0)
int(0)
Hello World.
int(0)
