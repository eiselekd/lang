// koka generated module: "main"
if (typeof define !== 'function') { var define = require('amdefine')(module) }
define(['./std_core', './std_path', './std_regex', './common', './options', './storage', './driver', './mathStaticRun', './runLatex', './version'], function($std_core, $std_path, $std_regex, $common, $options, $storage, $driver, $mathStaticRun, $runLatex, $version) {
"use strict";
 
// koka declarations:
var runners = $driver.Runners($runLatex.runPdfLaTeX, $runLatex.runBibtex, $mathStaticRun.runMathStatic, $runLatex.runZip);
function markdownFiles(args, action)  /* (args : string, action : (string, string, string, options/options) -> io ()) -> io () */  {
  var mbopts = $options.parseOptions($version.version, args);
  if (mbopts == null) {
    return $std_core._unit_;
  }
  else {
    var _x0 = (($options.outputDir(mbopts.unJust) !== "") && !($storage.fexistsSync($options.outputDir(mbopts.unJust))));
    if (_x0) {
      $storage.mkdirp($options.outputDir(mbopts.unJust), undefined);
    }
    else {
      $std_core._unit_;
    }
    return $std_core.foreach($options.inputs(mbopts.unJust), function(input0  /* string */ ) {  var _x0 = ($std_path.extname(input0) === ""); var input = (_x0) ? (input0 + ".mdk") : input0; var outName = $driver.outputName(input, mbopts.unJust); var _x0 = $options.verbose($options.options(mbopts.unJust)) > 0; if (_x0) {  $std_core.println(("process: " + (input + (" -> " + outName))));} else {  $std_core._unit_;} var _x0 = $storage.tryReadTextFile(input, undefined); if (_x0._tag === 1) {  return $std_core.println(("error: unable to read: " + input));} else {  return $driver.processContent(input, outName, _x0.left, mbopts.unJust, true, runners, action);}});
  }
}
function main()  /* () -> io () */  {
  return markdownFiles("", function(html  /* string */ , inName  /* string */ , outName  /* string */ , opts  /* options/options */ ) {  return $std_core._unit_;});
}
function test(s, moreargs)  /* (s : ?string, moreargs : ?string) -> io () */  {
  var s_543 = (s !== undefined) ? s : "code_blocks";
  var moreargs_547 = (moreargs !== undefined) ? moreargs : "";
  if (((s_543).indexOf($std_path.sep) >= 0)) {
    var root = $std_path.combine("test", s_543);
  }
  else {
    var root = $std_path.combine_1($std_core.Cons("test", $std_core.Cons("new", $std_core.Cons(s_543, $std_core.Nil))));
  }
  var input = (root + ".text");
  var outputDir = "test/out";
  var target = (root + ".html");
  return markdownFiles(("-v --tex --installdir=src --odir=" + (outputDir + (" " + (moreargs_547 + (" " + input))))), function(outText  /* string */ , __input  /* string */ , output  /* string */ , __options  /* options/options */ ) {  var targetText = $storage.readTextFileDef(target, "", undefined); var outStrip = $std_regex.replaceAll_1(outText, $std_regex.regex("\\s", undefined, undefined), "", undefined); var targetStrip = $std_regex.replaceAll_1(targetText, $std_regex.regex("\\s", undefined, undefined), "", undefined); if ((outStrip !== targetStrip)) {  $std_core.trace("\n*** test failed ***"); var i = { value: 0 }; $std_core.$while(function() {  return ((outStrip)[((i).value)] === (targetStrip)[((i).value)]);}, function() {  return ((i).value = ((((i).value) + 1)|0));}); $std_core.trace(("position: " + $std_core.show_1(((i).value)))); var preN = 20; ((i).value = $std_core.max(0, ((((i).value) - preN)|0))); $std_core.trace(("inferred: " + ($std_core.substr_1(outStrip, ((i).value), preN) + (" " + ($std_core.substr_1(outStrip, ((((i).value) + preN)|0), 40) + " ..."))))); $std_core.trace(("expected: " + ($std_core.substr_1(targetStrip, ((i).value), preN) + (" " + ($std_core.substr_1(targetStrip, ((((i).value) + preN)|0), 40) + "..."))))); return $std_core.trace("***");} else {  return $std_core.trace("*** test success (modulo whitespace)");}});
}
 
// koka main entry:
main();
 
// koka exports:
return { 'runners': runners, 'markdownFiles': markdownFiles, 'main': main, 'test': test };
});