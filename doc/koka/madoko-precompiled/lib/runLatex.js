// koka generated module: "runLatex"
if (typeof define !== 'function') { var define = require('amdefine')(module) }
define(['./std_core', './std_string', './std_regex', './std_path', './std_dict', './std_env', './common', './options', './storage', './process'], function($std_core, $std_string, $std_regex, $std_path, $std_dict, $std_env, $common, $options, $storage, $$process) {
"use strict";
 
// koka declarations:
var rxEndComment = $std_regex.regex("^(?:[^\\\\%]|\\\\.)*%mdk.*$", undefined, undefined);
var rxLineData = $std_regex.regex("^(?:[^\\\\%]|\\\\.)*% *(?:mdk-)?data-line=\\{([^\\}]*)\\} *$", undefined, undefined);
var rxLineNo = $std_regex.regex("^([^:]*:)?(\\d+)$", undefined, undefined);
function findLatexLineData(delta, lines)  /* (delta : int, lines : list<string>) -> maybe<string> */  { tailcall: while(1)
{
  if (lines == null) {
    return $std_core.Nothing;
  }
  else {
    var _x0 = $std_regex.find(lines.head, rxLineData, undefined);
    if (_x0 == null) {
      var newDelta = ($std_regex.contains(lines.head, rxEndComment, undefined)) ? delta : ((delta + 1)|0);
      {
        delta = newDelta;
        lines = lines.tail;
        continue tailcall;
      }
    }
    else {
      var last = $std_core.join_3($std_core.take($std_core.reverse($std_core.list_4($std_core.split_1($std_regex._index_($std_regex.groups(_x0.unJust), 1), ";"))), 1));
      var _x1 = $std_regex.find(last, rxLineNo, undefined);
      if (_x1 == null) {
        return $std_core.Nothing;
      }
      else {
        return $std_core.Just(($std_regex._index_($std_regex.groups(_x1.unJust), 1) + $std_core.show_1((($std_core.maybe($std_core.parseInt($std_regex._index_($std_regex.groups(_x1.unJust), 2), undefined), 0, $std_core.id) + delta)|0))));
      }
    }
  }
}}
function findLatexLine(src, lineNo, texSrc)  /* (src : string, lineNo : int, texSrc : string) -> maybe<string> */  {
  var searchLines = $std_core.reverse($std_core.take($std_core.list_4($std_core.lines(texSrc)), ((lineNo - 1)|0)));
  return findLatexLineData(0, searchLines);
}
var rxLatexLineNum = $std_regex.regex("(?:^|\\n)l\\.(\\d+) +[\\s\\S]*?(?=\\r?\\n\\r?\\n|$)", undefined, undefined);
function findLatexLineNo(texerr)  /* (texerr : string) -> maybe<(int, string)> */  {
  var _x0 = $std_regex.find(texerr, rxLatexLineNum, undefined);
  if (_x0 == null) {
    return $std_core.Nothing;
  }
  else {
    var _x1 = $std_core.parseInt($std_regex._index_($std_regex.groups(_x0.unJust), 1), undefined);
    if (_x1 == null) {
      return $std_core.Nothing;
    }
    else {
      return $std_core.Just($std_core._tuple2_(_x1.unJust, $std_core.substr_1(texerr, 0, $std_regex.next(_x0.unJust))));
    }
  }
}
var rxPackage = "\\((?:\\./)?([^\\)\\n\\r\\t]+) *(?:(\\)+) *)?";
var rxPackageEnd = ("(\\)+) *(?:" + (rxPackage + ")?"));
var rxPackageEndLoaded = "[^\\(\\r\\n]*?((?: loaded\\)+)+)";
 
// xypic package suffers from this
var rxPackageStart = rxPackage;
var rxPackageLine = $std_regex.regex(("^(?:\\] *)?(?:" + (rxPackageStart + ("|" + (rxPackageEnd + ("|" + (rxPackageEndLoaded + ")(?=$| *\\[\\d+(?:$|\\]))")))))), undefined, true);
function findLatexPackage(logout, pkgs)  /* (logout : string, pkgs : ?list<string>) -> maybe<string> */  { tailcall: while(1)
{
  var pkgs_2128 = (pkgs !== undefined) ? pkgs : $std_core.Nil;
  var _x0 = $std_regex.find(logout, rxPackageLine, undefined);
  if (_x0 == null) {
    if (pkgs_2128 != null) {
      return $std_core.Just($std_core.join_4($std_core.reverse(pkgs_2128), ";"));
    }
    else {
      return $std_core.Nothing;
    }
  }
  else {
    var _x1 = ($std_regex._index_($std_regex.groups(_x0.unJust), 6) !== "");
    if (_x1) {
      var newpkgs = $std_core.drop(pkgs_2128, ((")") ? (($std_regex._index_($std_regex.groups(_x0.unJust), 6)).match(new RegExp((")").replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'))||[]).length : 0));
    }
    else {
      var pkg = ($std_regex._index_($std_regex.groups(_x0.unJust), 1) + $std_regex._index_($std_regex.groups(_x0.unJust), 4));
      var dropBefore = ($std_regex._index_($std_regex.groups(_x0.unJust), 3)).length;
      var dropAfter = ((($std_regex._index_($std_regex.groups(_x0.unJust), 2)).length + ($std_regex._index_($std_regex.groups(_x0.unJust), 5)).length)|0);
      var pkgs0 = $std_core.drop(pkgs_2128, dropBefore);
      var pkgs1 = ($std_core.isEmpty(pkg)) ? pkgs0 : $std_core.Cons(pkg, pkgs0);
      var pkgs2 = $std_core.drop(pkgs1, dropAfter);
      var newpkgs = pkgs2;
    }
    {
      var _x1 = ((logout).substr($std_regex.next(_x0.unJust) >=1 ? $std_regex.next(_x0.unJust) : 1));
      var _x2 = newpkgs;
      logout = _x1;
      pkgs = _x2;
      continue tailcall;
    }
  }
}}
function findOffsets(source, pat)  /* (source : string, pat : string) -> list<int> */  {
  var i0 = ((source).indexOf(pat));
  var i1 = ((source).lastIndexOf(pat));
  if (i0 < 0) {
    return $std_core.Nil;
  }
  else {
    if (i0 === i1) {
      return $std_core.Cons(i0, $std_core.Nil);
    }
    else {
      return $std_core.Cons(i0, $std_core.Cons(i1, $std_core.Nil));
    }
  }
}
var rxStrip = $std_regex.regex("(\\\\[\\w/\\-\\r\\n]*(\\[[^\\]\\r\\n]*\\])?|\\^\\^.|\'\\w*|\\}\\{[lrct]\\}|\\]\\{[^\\]\\r\\n]*\\}|[^a-zA-Z])+", undefined, undefined);
function latexStrip(s)  /* (s : string) -> string */  {
  return $std_regex.replaceAll_1(s, rxStrip, "", undefined);
}
function findLines(source, pat)  /* (source : string, pat : string) -> list<int> */  {
  var slines = $std_core.map_3($std_core.list_4($std_core.lines(source)), function(line  /* string */ ) {  return latexStrip(line);});
  var slens = $std_core.map_3(slines, function(s  /* string */ ) {  return (s).length;});
  var offsets = findOffsets($std_core.join_3(slines), pat);
  return $std_core.map_3(offsets, function(ofs  /* int */ ) {  var total = { value: 0 }; var current = { value: 1 }; $std_core.foreachUntil(slens, function(len  /* int */ ) {  ((total).value = ((((total).value) + len)|0)); var _x0 = ((total).value) > ofs; if (_x0) {  return $std_core.Just(((current).value));} else {  $std_core._unit_;} ((current).value = ((((current).value) + 1)|0)); return $std_core.Nothing;}); return ((current).value);});
}
var latexEnv = $std_core.Cons($std_core._tuple2_("max_print_line", "1000"), $std_core.Cons($std_core._tuple2_("error_line", "250"), $std_core.Cons($std_core._tuple2_("half_error_line", "230"), $std_core.Nil)));
function latexGetLine(src, lineNo)  /* (src : vector<string>, lineNo : int) -> string */  {
  var _x0 = (lineNo <= 0 || lineNo > (src).length);
  return (_x0) ? "" : $std_core.$catch(function() {  return (src)[((lineNo - 1)|0)];}, function(___lp_432_comma_35_rp_  /* exception */ ) {  return "";});
}
var rxBegin = $std_regex.regex("^\\\\begin|^\\s*(\\\\?\\{})*$", undefined, true);
var rxEnd = $std_regex.regex("^\\\\end|^\\s*(\\\\?\\}\\s*)*$", undefined, true);
function latexFindLine(src, lineNo, direction)  /* (src : vector<string>, lineNo : int, direction : ?int) -> div string */  { tailcall: while(1)
{
  var direction_4934 = (direction !== undefined) ? direction : 0;
  var line = latexGetLine(src, lineNo);
  var _x0 = ($std_regex.contains(line, rxEnd, undefined) && direction_4934 <= 0);
  if (_x0) {
    {
      var _x1 = ((lineNo - 1)|0);
      var _x2 = $std_core._tilde_(1);
      lineNo = _x1;
      direction = _x2;
      continue tailcall;
    }
  }
  else {
    var _x3 = ($std_regex.contains(line, rxBegin, undefined) && direction_4934 >= 0);
    if (_x3) {
      {
        var _x4 = ((lineNo + 1)|0);
        var _x5 = 1;
        lineNo = _x4;
        direction = _x5;
        continue tailcall;
      }
    }
    else {
      return line;
    }
  }
}}
function latexFindLineByMatch(texLine, source)  /* (texLine : string, source : string) -> maybe<string> */  {
  var stripLine = $std_core.substr_1(latexStrip(texLine), 0, 16);
  var _x0 = (stripLine).length < 3;
  if (_x0) {
    return $std_core.Nothing;
  }
  else {
    var _x1 = findLines(source, stripLine);
    if (_x1 != null) {
      var _x2 = ($std_core.isNil(_x1.tail)) ? "" : " (?)";
      return $std_core.Just(($std_core.show_1(_x1.head) + _x2));
    }
    else {
      return $std_core.Nothing;
    }
  }
}
var latexSandboxEnv = $std_core.Cons($std_core._tuple2_("openin_any", "p"), $std_core.Cons($std_core._tuple2_("openout_any", "p"), $std_core.Cons($std_core._tuple2_("shell_escape", "f"), $std_core.Cons($std_core._tuple2_("parse_first_line", "f"), $std_core.Nil))));
var rxLatexBrackets = $std_regex.regex("(\\s|[\\[\\]])+$", undefined, undefined);
var rxLatexLine = $std_regex.regex("lines?[\\s:]*(\\d+).*(?:\\r?\\n([\\s\\S]*))?", undefined, undefined);
var rxLatexOutput = $std_regex.regex("^Output written on .*", undefined, true);
var rxLatexOutputActive = $std_regex.regex("\\output is active\\s*$", undefined, undefined);
 
// only match single line
var rxLatexPage = $std_regex.regex("\\s\\[(\\d+)\\s*\\]", undefined, undefined);
var rxLatexWarning = $std_regex.regex("^(Overfull|Underfull|[\\w ]*[Ww]arning:)[\\s\\S]*?\\n *$", undefined, true);
function latexShowWarnings(texFile, logout, content, opts)  /* (texFile : string, logout : string, content : string, opts : options/options) -> io () */  {
  var _x0 = $options.verbose(opts) < 2;
  if (_x0) {
    var _x1 = $options.verbose(opts) >= 1;
    if (_x1) {
      var _x2 = $std_regex.find(logout, rxLatexOutput, undefined);
      if (_x2 != null) {
        $options.print(opts, $std_regex.matched(_x2.unJust), 1);
      }
      else {
        $std_core._unit_;
      }
    }
    else {
      $std_core._unit_;
    }
    return $std_core._unit_;
  }
  else {
    $std_core._unit_;
  }
  var warnings = $std_core.map_3($std_core.list_4($std_regex.findAll(logout, rxLatexWarning, undefined)), function(cap0  /* std/regex/matched */ ) {  return $std_core._tuple2_($std_regex.index(cap0), $std_regex.matched(cap0));});
  if ($std_core.isNil(warnings)) {
    return $std_core._unit_;
  }
  else {
    $std_core._unit_;
  }
  var texlines = $std_core.lines($storage.readTextFileDef(texFile, "", undefined));
  $std_core.println("");
  $std_core.foreach(warnings, function(iw  /* (int, string) */ ) {  var warning = $std_regex.replace_1(iw.snd, rxLatexBrackets, "", undefined); var _x0 = $std_regex.find(warning, rxLatexLine, undefined); if (_x0 == null) {  var _x1 = $std_regex.find(warning, rxLatexOutputActive, undefined); if (_x1 != null) {  var _x2 = $std_core.reverse($std_core.list_4($std_regex.findAll($std_core.substr_1(logout, 0, iw.fst), rxLatexPage, undefined))); if (_x2 != null) {  var page = $std_core.show_1((($std_core.maybe($std_core.parseInt($std_regex._index_($std_regex.groups(_x2.head), 1), undefined), 0, $std_core.id) + 1)|0));} else {  var page = "1";} var pre = ("> warning: page " + (page + "\n"));} else {  var pre = "";} var warn = (pre + warning);} else {  var _x1 = (latexStrip($std_regex._index_($std_regex.groups(_x0.unJust), 2)) !== ""); if (_x1) {  var line = $std_regex._index_($std_regex.groups(_x0.unJust), 2);} else {  var lineNo = $std_core.maybe($std_core.parseInt($std_regex._index_($std_regex.groups(_x0.unJust), 1), undefined), 0, $std_core.id); var line = latexFindLine(texlines, lineNo, undefined);} var _x1 = latexFindLineByMatch(line, content); if (_x1 == null) {  var _x2 = $options.verbose(opts) >= 4; if (_x2) {  $options.printErr(opts, ("unable to find line: \n> " + (latexStrip(line) + "\n")), undefined);} else {  $std_core._unit_;} var pre0 = "";} else {  var pre0 = ("> warning: source line: " + (_x1.unJust + "\n"));} var _x2 = (($std_regex._index_($std_regex.groups(_x0.unJust), 2) === "") && (line !== "")); if (_x2) {  var _x1 = ("\n> " + $std_core.substr_1(line, 0, 74));} else {  var _x1 = "";} var warn = (pre0 + (warning + _x1));} return $options.printErr(opts, (warn + "\n"), undefined);});
  var _x0 = $std_regex.find(logout, rxLatexOutput, undefined);
  if (_x0 != null) {
    return $options.printErr(opts, $std_regex.matched(_x0.unJust), undefined);
  }
  else {
    return ($std_core.isCons(warnings)) ? $options.printErr(opts, "end of latex warnings.", undefined) : $std_core._unit_;
  }
}
var rxLatexSubErr = $std_regex.regex("The font \"[^\"\\n\\r]*\" cannot be found\\.", true, true);
function nicifyLatexErr(err)  /* (err : string) -> string */  {
  var _x0 = $std_regex.find(err, rxLatexSubErr, undefined);
  if (_x0 == null) {
    return err;
  }
  else {
    return ($std_regex.matched(_x0.unJust) + "\n");
  }
}
 
// normalize a file name: needed for latex
function norm(fname)  /* (fname : string) -> string */  {
  return (fname).replace(new RegExp(("\\").replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),"/");
}
var rxBibtexLineinfo = "---line *(\\d+) *of file *(.*)";
var rxBibtexError = $std_regex.regex(("^ *(.+?)" + (rxBibtexLineinfo + "((?:\\s*:.*)*)")), undefined, true);
var rxBibtexNoStyle = $std_regex.regex(("^ *I couldn\'t open style file *(.*)\\s*" + rxBibtexLineinfo), undefined, true);
function runBibtex1(bibFile, opts, $continue)  /* forall<e> (bibFile : string, opts : options/options, continue : (bool) -> <io|e> ()) -> <io|e> () */  {
  $options.print(opts, "running bibtex to generate bibliography...", undefined);
  var xbibFile = $storage.checkSandbox(bibFile);
  var bibtexCmd = ($$process.quote($options.bibtex(opts)) + (" " + $$process.quote($std_path.basename(xbibFile))));
  $options.print(opts, ("> " + bibtexCmd), 3);
  return $$process.system(bibtexCmd, function(err  /* int */ , stdout  /* string */ , stderr  /* string */ ) {  if (err !== 0) {  $options.printErr(opts, ("error while running: > " + bibtexCmd), undefined); return $$process.system(($$process.quote($options.bibtex(opts)) + " -version"), function(errx  /* int */ , stdoutx  /* string */ , stderrx  /* string */ ) {  if (errx !== 0) {  $options.printErr(opts, ("error: could not find bibtex: \"" + (bibtexCmd + ("\"\n" + "\n  hint: Set the \'BibTeX: ...\' key or give the \'--bib=<cmd>\' command line option.\n"))), undefined);} else {  var allout = (stdout + stderr); $options.printErr(opts, ("error: bibtex:\n" + $std_string.indent(allout, 2, undefined)), undefined); var _x0 = $std_regex.find(allout, rxBibtexNoStyle, undefined); if (_x0 == null) {  $std_core._unit_;} else {  $options.printErr(opts, ("error: source line: " + ($std_regex._index_($std_regex.groups(_x0.unJust), 3) + (":" + ($std_regex._index_($std_regex.groups(_x0.unJust), 2) + ("\n  cannot find bibliography style file \'" + ($std_regex._index_($std_regex.groups(_x0.unJust), 1) + ("\'" + ("\n  hint: perhaps put the file in the document directory and use the" + ("\n        \'Bib style: " + ($std_path.noext($std_regex._index_($std_regex.groups(_x0.unJust), 1)) + ".bst\' metadata (with the \'.bst\' extension).\n")))))))))), undefined);} $std_core.foreach_2($std_regex.findAll(allout, rxBibtexError, undefined), function(cap0  /* std/regex/matched */ ) {  return $options.printErr(opts, ("error: source line: " + ($std_regex._index_($std_regex.groups(cap0), 3) + (":" + ($std_regex._index_($std_regex.groups(cap0), 2) + ("\n  " + ($std_string.indent(($std_regex._index_($std_regex.groups(cap0), 1) + $common.unindent($std_regex._index_($std_regex.groups(cap0), 4))), 2, undefined) + "\n")))))), undefined);});} return $continue(true);}, undefined, undefined, undefined);} else {  var _x0 = ($options.verbose(opts) >= 1 && !(((stdout === "") && (stderr === "")))); if (_x0) {  $options.printErr(opts, (stdout + stderr), undefined);} else {  $std_core._unit_;} return $continue(false);}}, $options.processTimeout(opts), $std_path.dirname(bibFile), undefined);
}
function runBibtex(bibFiles, opts, $continue)  /* forall<e> (bibFiles : list<string>, opts : options/options, continue : (bool) -> <io|e> ()) -> <io|e> () */  {
  return (bibFiles == null) ? $continue(false) : runBibtex1(bibFiles.head, opts, function(err  /* bool */ ) {  var _x0 = (err) ? function(___lp_56_comma_50_rp_  /* bool */ ) {  return $continue(true);} : $continue; return runBibtex(bibFiles.tail, opts, _x0);});
}
var rxLatexErr = $std_regex.regex("(?:^! LaTeX Error:.*|^\\*\\* ERROR .*|^(?:!|Runaway argument\\?))[\\s\\S]*?(?=\\r?\\n\\r?\\n\\r?\\n|^\\s*(?:Here is how much of \\w*TeX\'s memory you used:|The control sequence))", true, true);
var rxLatexRerun = $std_regex.regex("^((Package|Latex) .*?warning\\b.*?|\\(\\w+\\) +)Rerun\\b", true, true);
function runLaTeX(srcFile, texFile, texCmd, extraArgs, opts, content, notfound, runCount, showWarnings, $continue)  /* (srcFile : string, texFile : string, texCmd : string, extraArgs : string, opts : options/options, content : string, notfound : string, runCount : ?int, showWarnings : ?bool, continue : (err : int) -> io ()) -> io () */  {
  var runCount_11189 = (runCount !== undefined) ? runCount : 0;
  var showWarnings_11193 = (showWarnings !== undefined) ? showWarnings : true;
  var dir = norm($std_path.dirname(texFile));
  var _x0 = ($std_core.bool_2(extraArgs)) ? (" " + extraArgs) : "";
  var _x1 = ((dir === "")) ? "." : dir;
  var _x2 = ($options.sandbox(opts)) ? " --no-parse-first-line --no-shell-escape" : "";
  var latexCmd = ($$process.quote(texCmd) + (_x0 + (" --output-directory=" + ($$process.quote(_x1) + (" --interaction=batchmode" + (_x2 + (" " + $$process.quote(norm(texFile)))))))));
  var _x0 = (runCount_11189 === 0 && $options.rebuild(opts));
  if (_x0) {
    $storage.tryUnlinkSync($std_path.changeExt(texFile, ".aux"));
  }
  else {
    $std_core._unit_;
  }
  $options.print(opts, ("> " + latexCmd), 3);
  var _x1 = ($options.sandbox(opts)) ? latexSandboxEnv : $std_core.Nil;
  return $$process.system(latexCmd, function(err  /* int */ , stdout  /* string */ , stderr  /* string */ ) {  var logout = $storage.readTextFileDef($std_path.changeExt(texFile, ".log"), "", undefined); if (err !== 0) {  $options.printErr(opts, ("error while running: \n> " + latexCmd), undefined); return $$process.system(($$process.quote(texCmd) + " -version"), function(errx  /* int */ , stdoutx  /* string */ , stderrx  /* string */ ) {  if (errx !== 0) {  $options.printErr(opts, ("error: could not find program: \"" + (texCmd + "\"")), undefined); ((notfound !== "")) ? $options.printErr(opts, notfound, undefined) : $std_core._unit_;} else {  var errmsg = ("error: latex:\n" + (stdout + stderr)); var _x0 = $std_regex.find(logout, rxLatexErr, undefined); if (_x0 != null) {  var _x1 = findLatexLineNo($std_regex.matched(_x0.unJust)); if (_x1 != null) {  var texContent = $storage.readTextFileDef(texFile, "", undefined); var _x2 = findLatexPackage($std_core.substr_1(logout, 0, $std_regex.index(_x0.unJust)), undefined); if (_x2 != null) {  $options.printErr(opts, ("error: source line: " + (_x2.unJust + (":" + $std_core.show_1(_x1.unJust.fst)))), undefined);} else {  var _x3 = findLatexLine(content, _x1.unJust.fst, texContent); if (_x3 != null) {  $options.printErr(opts, ("error: source line: " + _x3.unJust), undefined);} else {  $std_core._unit_;}} $options.printErr(opts, nicifyLatexErr(_x1.unJust.snd), undefined);} else {  $options.printErr(opts, errmsg, undefined);}} else {  $options.printErr(opts, errmsg, undefined);} $options.printErr(opts, ("log written at: " + $std_path.changeExt(texFile, ".log")), undefined);} return $continue(err);}, undefined, undefined, undefined);} else {  var _x0 = (runCount_11189 <= 2 && $std_regex.contains(logout, rxLatexRerun, undefined)); if (_x0) {  $options.print(opts, "rerun to resolve references...", undefined); return runLaTeX(srcFile, texFile, texCmd, extraArgs, opts, content, notfound, ((runCount_11189 + 1)|0), showWarnings_11193, $continue);} else {  (showWarnings_11193) ? latexShowWarnings(texFile, logout, content, opts) : $std_core._unit_; return $continue(0);}}}, $options.processTimeout(opts), $std_path.dirname(srcFile), $std_core._plus__3($std_dict.list_1($std_env.env), $std_core._plus__3(latexEnv, _x1)));
}
function runPdfLaTeX(srcName, texFile, opts, content, $continue)  /* (srcName : string, texFile : string, opts : options/options, content : string, continue : (int) -> io ()) -> io () */  {
  $options.print(opts, ("running " + ($std_path.stemname($options.getPdfLatex(opts)) + " to generate pdf...")), undefined);
  var notfound = "set the \'Pdf Latex: <cmd>\' key in the metadata?";
  return runLaTeX(srcName, texFile, $options.getPdfLatex(opts), "", opts, content, notfound, 0, true, $continue);
}
function runZip(files, zipFile, opts, $continue)  /* forall<e> (files : list<string>, zipFile : string, opts : options/options, continue : (bool) -> <io|e> ()) -> <io|e> () */  {
  $options.print(opts, "running zip to generate tex zip...", undefined);
  var outdir = $std_path.dirname(zipFile);
  function adjust(fname)  /* (fname : string) -> io string */  {
    if ($std_core.startsWith(fname, outdir)) {
      var name = ((fname).substr((((outdir).length + 1)|0)));
    }
    else {
      var name = fname;
    }
    return $$process.quote($storage.checkSandbox(name));
  }
  var zipCmd = ($$process.quote($options.zip(opts)) + (" -9 " + (adjust(zipFile) + (" " + $std_core.join_4($std_core.map_3(files, adjust), " ")))));
  $storage.tryUnlinkSync($storage.checkSandbox(zipFile));
  $options.print(opts, ("> " + zipCmd), undefined);
  return $$process.system(zipCmd, function(err  /* int */ , stdout  /* string */ , stderr  /* string */ ) {  if (err !== 0) {  $options.printErr(opts, ("error while running: > " + zipCmd), undefined); var allout = (stdout + stderr); $options.printErr(opts, ("error: zip:\n" + $std_string.indent(allout, 2, undefined)), undefined);} else {  var _x0 = ($options.verbose(opts) >= 1 && !(((stdout === "") && (stderr === "")))); if (_x0) {  $options.printErr(opts, (stdout + stderr), undefined);} else {  $std_core._unit_;}} return $continue(err !== 0);}, $options.processTimeout(opts), outdir, undefined);
}
var rxLatexEndSnippet = $std_regex.regex("\\\\end{md(Inline|Display)?Snippet}", undefined, undefined);
 
// koka exports:
return { 'rxEndComment': rxEndComment, 'rxLineData': rxLineData, 'rxLineNo': rxLineNo, 'findLatexLineData': findLatexLineData, 'findLatexLine': findLatexLine, 'rxLatexLineNum': rxLatexLineNum, 'findLatexLineNo': findLatexLineNo, 'rxPackage': rxPackage, 'rxPackageEnd': rxPackageEnd, 'rxPackageEndLoaded': rxPackageEndLoaded, 'rxPackageStart': rxPackageStart, 'rxPackageLine': rxPackageLine, 'findLatexPackage': findLatexPackage, 'findOffsets': findOffsets, 'rxStrip': rxStrip, 'latexStrip': latexStrip, 'findLines': findLines, 'latexEnv': latexEnv, 'latexGetLine': latexGetLine, 'rxBegin': rxBegin, 'rxEnd': rxEnd, 'latexFindLine': latexFindLine, 'latexFindLineByMatch': latexFindLineByMatch, 'latexSandboxEnv': latexSandboxEnv, 'rxLatexBrackets': rxLatexBrackets, 'rxLatexLine': rxLatexLine, 'rxLatexOutput': rxLatexOutput, 'rxLatexOutputActive': rxLatexOutputActive, 'rxLatexPage': rxLatexPage, 'rxLatexWarning': rxLatexWarning, 'latexShowWarnings': latexShowWarnings, 'rxLatexSubErr': rxLatexSubErr, 'nicifyLatexErr': nicifyLatexErr, 'norm': norm, 'rxBibtexLineinfo': rxBibtexLineinfo, 'rxBibtexError': rxBibtexError, 'rxBibtexNoStyle': rxBibtexNoStyle, 'runBibtex1': runBibtex1, 'runBibtex': runBibtex, 'rxLatexErr': rxLatexErr, 'rxLatexRerun': rxLatexRerun, 'runLaTeX': runLaTeX, 'runPdfLaTeX': runPdfLaTeX, 'runZip': runZip, 'rxLatexEndSnippet': rxLatexEndSnippet };
});