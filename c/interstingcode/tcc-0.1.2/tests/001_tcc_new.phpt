--TEST--
Check tcc_new
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  if($tcc) echo 'ok';
?>
--EXPECT--
ok
