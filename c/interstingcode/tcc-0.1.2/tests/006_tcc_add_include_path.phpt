--TEST--
Check tcc_add_include_path()
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_add_include_path($tcc, 'tests');
  var_dump($ret);
  $ret = tcc_compile_string($tcc, '
    #include "hello.h"
    int main(){hello(); return 0;}
  ');
  var_dump($ret);
  $ret = tcc_run($tcc);
  var_dump($ret);
?>
--EXPECT--
int(0)
int(0)
Hello World.
int(0)
