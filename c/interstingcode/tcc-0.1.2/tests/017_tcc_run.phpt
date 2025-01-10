--TEST--
Check tcc_run() - error
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $buf = 'int main(){undefined_function(); return 0;}';
  $tcc = tcc_new();
  $ret = tcc_compile_string($tcc, $buf);
  var_dump($ret);
  $ret = tcc_run($tcc);
?>
--EXPECT--
int(0)
<string>:1: undefined symbol 'undefined_function'
