--TEST--
Check tcc_set_output_type()
--SKIPIF--
<?php if (!extension_loaded("tcc")) print "skip"; ?>
--FILE--
<?php 
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_MEMORY);
  var_dump($ret);
  unset($tcc);
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_EXE);
  var_dump($ret);
  unset($tcc);
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_DLL);
  var_dump($ret);
  unset($tcc);
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_OBJ);
  var_dump($ret);
  unset($tcc);
  $tcc = tcc_new();
  $ret = tcc_set_output_type($tcc, TCC_OUTPUT_PREPROCESS);
  var_dump($ret);
  unset($tcc);
?>
--EXPECT--
int(0)
int(0)
int(0)
int(0)
int(0)
