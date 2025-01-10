--TEST--
Check tcc_add_file()
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_add_file($tcc, 'tests/hello.c');
  var_dump($ret);
  $ret = tcc_run($tcc);
  var_dump($ret);
?>
--EXPECT--
int(0)
Hello World.
int(0)
