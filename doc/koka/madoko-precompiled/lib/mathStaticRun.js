// koka generated module: "mathStaticRun"
if (typeof define !== 'function') { var define = require('amdefine')(module) }
define(['./std_core', './std_string', './std_regex', './std_path', './std_dict', './std_env', './common', './options', './storage', './process', './mathParse', './mathStatic', './runLatex'], function($std_core, $std_string, $std_regex, $std_path, $std_dict, $std_env, $common, $options, $storage, $$process, $mathParse, $mathStatic, $runLatex) {
"use strict";
 
// koka declarations:
function analyzeImagePng(imageFile, opts)  /* (imageFile : string, opts : options/options) -> io (double, double, int, string) */  {
  var _x0 = $storage.tryReadFileSync(imageFile);
  if (_x0 == null) {
    $options.printErr(opts, ("error: cannot read: " + imageFile), undefined);
    return $std_core._tuple4_(0.0, 0.0, 0, "");
  }
  else {
    var base64 = $storage.toBase64(_x0.unJust);
    var pxwidth = $storage.readInt4(_x0.unJust, 16, true);
    var pxheight = $storage.readInt4(_x0.unJust, 20, true);
    var _x2 = $options.dpi($options.math(opts)) > 0;
    if (_x2) {
      var _x1 = ($options.dpi($options.math(opts)));
    }
    else {
      var _x1 = 72.27;
    }
    var ppt = (_x1 / 72.27);
    var iheight = ((pxheight) / ppt);
    var iwidth = ((pxwidth) / ppt);
    var pngprefix = "data:image/png;base64,";
    var _x1 = ((base64 !== "") && (((base64).length + (pngprefix).length)|0) < $options.embedLimit($options.math(opts)));
    var embed = (_x1) ? (pngprefix + base64) : "";
    var size = ($std_core.isEmpty(embed)) ? $storage.length(_x0.unJust) : (embed).length;
    return $std_core._tuple4_(iwidth, iheight, size, embed);
  }
}
var rxSvgId = $std_regex.regex("\\b(id=[\'\"](?:g)|xlink:href=[\'\"]#g)(?=\\d)", undefined, undefined);
 
// In order for glyphs to not be shared across pdf/dvi runs, we need to prefix
// identifiers so they are distinct and do not become shared paths.
// Glyph id's are of the form "g<fontid>-<charid>"
function encodeGlyphIds(svg, mode)  /* (svg : string, mode : common/mathkind) -> string */  {
  if ($common.isPlain(mode)) {
    return svg;
  }
  else {
    $std_core._unit_;
  }
  return $std_regex.replaceAll(svg, rxSvgId, function(cap  /* std/regex/matched */ ) {  return ($std_regex.matched(cap) + $common.show_3(mode));}, undefined);
}
var rxSvgHeight = $std_regex.regex("\\bheight=\'(\\d+)(?:\\.(\\d+))?(pt)?\'", undefined, undefined);
var rxSvgWidth = $std_regex.regex("\\bwidth=\'(\\d+)(?:\\.(\\d+))?(pt)?\'", undefined, undefined);
var rxSvgGroupId = $std_regex.regex("(<g\\s+id=[\'\"])page\\d+", undefined, undefined);
var rxSvgTextStyle = $std_regex.regex("\\btext\\.f\\d+\\s*\\{\\s*font", undefined, undefined);
 
// Scope CSS styles inside an SVG generated image such that we
// can embed multiple inside one HTML file. We use first 6 letters of
// the digest to uniquely name the SVG group from `page<no>` to `g-<digest>`.
function scopeStyles(svg, digest)  /* (svg : string, digest : string) -> string */  {
  var groupId = ("math-" + $std_core.substr_1(digest, 0, 6));
  var pre = ("#" + groupId);
  return $std_regex.replace_1($std_regex.replaceAll_1(svg, rxSvgTextStyle, (pre + " $&"), undefined), rxSvgGroupId, ("$1" + groupId), undefined);
}
var rxComment = $std_regex.regex("<!--[\\s\\S]*?-->|<\\?xml[\\s\\S]*?\\?>|\\bversion=\'1.1\'|\\bxmlns=\'http://www.w3.org/2000/svg\'|\\bxmlns:xlink=\'http://www.w3.org/1999/xlink\'", undefined, undefined);
var rxNl = $std_regex.regex("( *\\r?\\n)+(?=[ <])", undefined, undefined);
var rxSpaces = $std_regex.regex("\\s+", undefined, undefined);
 
// Remove unnecessary parts of an svg to reduce the size (important since there
// can be thousands in math-heavy documents)
function svgCompress(svg)  /* (svg : string) -> string */  {
  return $std_string.trim($std_regex.replaceAll_1($std_regex.replaceAll_1($std_regex.replaceAll_1(svg, rxComment, "", undefined), rxNl, "", undefined), rxSpaces, " ", undefined));
}
function svgExtractDim(svg, rx)  /* (svg : string, rx : std/regex/regex) -> (double, string) */  {
  var _x0 = $std_regex.find(svg, rx, undefined);
  if (_x0 == null) {
    return $std_core._tuple2_(0.0, svg);
  }
  else {
    return $std_core._tuple2_($mathParse.dimension($std_regex._index_($std_regex.groups(_x0.unJust), 1), $std_regex._index_($std_regex.groups(_x0.unJust), 2)), ($std_core.substr_1(svg, 0, $std_regex.index(_x0.unJust)) + ((svg).substr($std_regex.next(_x0.unJust)))));
  }
}
function analyzeImageSvg(imageFile, opts, mode, digest)  /* (imageFile : string, opts : options/options, mode : common/mathkind, digest : string) -> io (double, double, int, string) */  {
  var _x0 = $storage.tryReadTextFile(imageFile, undefined);
  if (_x0._tag === 1) {
    $options.printErr(opts, ("error: cannot read: " + imageFile), undefined);
    return $std_core._tuple4_(0.0, 0.0, 0, "");
  }
  else {
    var _x1 = svgExtractDim(_x0.left, rxSvgHeight);
    var _x2 = svgExtractDim(_x1.snd, rxSvgWidth);
    var svg = svgCompress(_x2.snd);
    var encoded = ("data:image/svg+xml;charset=utf8," + scopeStyles(encodeGlyphIds(svg, mode), digest));
    var _x3 = (encoded).length < $options.embedLimit($options.math(opts));
    var embed = (_x3) ? encoded : "";
    var size = ($std_core.isEmpty(embed)) ? (svg).length : (embed).length;
    return $std_core._tuple4_(_x2.fst, _x1.fst, size, embed);
  }
}
 
// fix up the tex file if we switch to PNG mode so PS specials are rendered correctly in TikZ/Pgf
function fileRenderPng(fname)  /* (fname : string) -> io () */  {
  var content = $storage.readTextFileDef(fname, "", false);
  if ($std_core.notEmpty(content)) {
    var contentPng = (content).replace(new RegExp(("\\mdmathrender{svg}").replace(/[\\\$\^*+\-{}?().]/g,'\\$&'),'g'),"\\mdmathrender{png}");
    $storage.tryWriteTextFile(fname, contentPng);
    return $std_core._unit_;
  }
  else {
    return $std_core._unit_;
  }
}
var rxVersion = $std_regex.regex("(\\d+)(?:\\.(\\d+)?(?:\\.\\d*)?)?", undefined, undefined);
function checkDviSvgVersion(opts, texNamePlain, texNameFull, cont)  /* (opts : options/options, texNamePlain : string, texNameFull : string, cont : (options/options) -> io ()) -> io () */  {
  var _x0 = ($common.isSvg($options.getMathRender_1($options.math(opts))) || $common.isSvg($options.getMathRenderFull($options.math(opts))));
  if (_x0) {
    var dvisvgm = $$process.quote($options.dvisvg($options.math(opts)));
    return $$process.system((dvisvgm + " --version"), function(err  /* int */ , stdout  /* string */ , stderr  /* string */ ) {  var _x1 = $std_regex.find(stdout, rxVersion, undefined); if (_x1 == null) {  var version = 0;} else {  var version = (($std_core.intMultiply(100,$std_core.parseIntDefault($std_regex._index_($std_regex.groups(_x1.unJust), 1), 0, undefined)) + $std_core.parseIntDefault($std_regex._index_($std_regex.groups(_x1.unJust), 2), 0, undefined))|0);} var _x1 = (err !== 0 || version < 116); if (_x1) {  $options.printErr(opts, ("warning: cannot use " + (dvisvgm + "; ensure you have at least version 1.16")), undefined); $options.printErr(opts, " the latest version is available at: http://dvisvgm.bplaced.net/downloads", undefined); $options.printErr(opts, " for Windows with TexLive 2016, see: madoko.net/doc/reference.html#dvisvgm", undefined); $options.printErr(opts, " switching to use png images for math (instead of svg)", undefined); fileRenderPng(texNamePlain); fileRenderPng(texNameFull); return cont($options._copy_1(opts, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, $options._copy($options.math(opts), undefined, $std_core.Just($common.Png), $std_core.Just($common.Png), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined));} else {  return cont(opts);}}, undefined, undefined, undefined);
  }
  else {
    return cont(opts);
  }
}
function concurrent(tasks, $continue)  /* (tasks : list<(cont : (int) -> io ()) -> io ()>, continue : (int) -> io ()) -> io () */  {
  if ($std_core.isNil(tasks)) {
    return $continue(0);
  }
  else {
    $std_core._unit_;
  }
  var count = { value: $std_core.length_3(tasks) };
  var lasterr = { value: 0 };
  function taskContinue(err)  /* (err : int) -> io () */  {
    ((count).value = ((((count).value) - 1)|0));
    (err !== 0) ? ((lasterr).value = err) : $std_core._unit_;
    var _x0 = ((count).value) <= 0;
    if (_x0) {
      return $continue(((lasterr).value));
    }
    else {
      return $std_core._unit_;
    }
  }
  return $std_core.foreach(tasks, function(task  /* (cont : (int) -> io ()) -> io () */ ) {  return $std_core.onFail(function() {  return ((count).value = ((((count).value) - 1)|0));}, function() {  return task(taskContinue);});});
}
function convertPages(pages)  /* (pages : mathStatic/pages) -> string */  {
  return ("[" + ($std_core.join_4($std_core.map_3(pages, function(rng  /* (int, int) */ ) {  if (rng.fst >= rng.snd) {  var _x0 = "";} else {  var _x0 = ("-" + $std_core.show_1(((rng.snd - 1)|0)));} return ($std_core.show_1(((rng.fst - 1)|0)) + _x0);}), ",") + "]"));
}
function convertSuffix(pages)  /* (pages : mathStatic/pages) -> string */  {
  if (pages != null) {
    if (pages.head.fst === pages.head.snd){
      return ("-" + $std_core.show_1(((pages.head.fst - 1)|0)));
    }
  }
  return "";
}
function dvipngPages(pages)  /* (pages : mathStatic/pages) -> string */  {
  return $std_core.join_4($std_core.map_3(pages, function(rng  /* (int, int) */ ) {  if (rng.fst >= rng.snd) {  var _x0 = "";} else {  var _x0 = ("-" + $std_core.show_1(rng.snd));} return ("-pp " + ($std_core.show_1(rng.fst) + _x0));}), " ");
}
function dvisvgPages(pages)  /* (pages : mathStatic/pages) -> string */  {
  return ("-p" + $std_core.join_4($std_core.map_3(pages, function(rng  /* (int, int) */ ) {  if (rng.fst >= rng.snd) {  var _x0 = "";} else {  var _x0 = ("-" + $std_core.show_1(rng.snd));} return ($std_core.show_1(rng.fst) + _x0);}), ","));
}
function extendDim(plainPages, fullPages, texNamePlain, texNameFull, plainPageAdj, plainBboxAdj, fullPageAdj, fullBboxAdj, dimxFile, opts)  /* (plainPages : mathStatic/pages, fullPages : mathStatic/pages, texNamePlain : string, texNameFull : string, plainPageAdj : int, plainBboxAdj : double, fullPageAdj : int, fullBboxAdj : double, dimxFile : string, opts : options/options) -> io string */  {
  var imageSize = { value: 0 };
  var imageCount = { value: 0 };
  var embedSize = { value: 0 };
  var embedCount = { value: 0 };
  var dim = $options.dim($options.math(opts));
  function dimLine(render, mode, fname, pages, pageAdj, bboxAdj, line)  /* (render : common/mathrender, mode : common/mathkind, fname : string, pages : mathStatic/pages, pageAdj : int, bboxAdj : double, line : string) -> io string */  {
    var _x0 = $std_regex.find(line, $mathParse.rxDimLine, undefined);
    if (_x0 == null) {
      return line;
    }
    else {
      var pageNo = $std_core.maybe($std_core.parseInt($std_regex._index_($std_regex.groups(_x0.unJust), 2), undefined), 0, $std_core.id);
      var digest = $std_regex._index_($std_regex.groups(_x0.unJust), 3);
      var imageStem = ($std_path.stemname(fname) + ("-" + $std_core.show_1(((pageNo + pageAdj)|0))));
      var imageName = $std_path.combine($options.imgDir($options.math(opts)), $std_path.changeExt(imageStem, $common.show_2(render)));
      var imageFile = $std_path.combine($std_path.dirname(dimxFile), imageName);
      var _x2 = $std_dict._lb__rb__2(dim, digest);
      match: {
        if (_x2 != null) {
          if (!($mathStatic.inside(pageNo, pages))){
            var _x1 = $std_core._tuple4_($common.iwidth(_x2.unJust), $common.iheight(_x2.unJust), $common.size(_x2.unJust), $common.originalData(_x2.unJust));
            break match;
          }
        }
        var res = (render === 1) ? analyzeImagePng(imageFile, opts) : analyzeImageSvg(imageFile, opts, mode, digest);
        var imageDigestFile = $std_path.combine($std_path.dirname(imageFile), ("math-" + (digest + $std_path.extname(imageFile))));
        $storage.tryUnlinkSync(imageDigestFile);
        var _x3 = (($std_core.field4(res) === "") && $std_core.thd_1(res) > 0);
        (_x3) ? $storage.rename(imageFile, imageDigestFile) : $storage.tryUnlinkSync(imageFile);
        var _x1 = res;
      }
      if ((_x1.field4 === "")) {
        ((imageSize).value = ((((imageSize).value) + _x1.thd)|0));
        ((imageCount).value = ((((imageCount).value) + 1)|0));
      }
      else {
        ((embedSize).value = ((((embedSize).value) + _x1.thd)|0));
        ((embedCount).value = ((((embedCount).value) + 1)|0));
      }
      var _x3 = ((_x1.field4 === "")) ? "" : (", " + _x1.field4);
      return ($std_regex._index_($std_regex.groups(_x0.unJust), 1) + ("," + ($std_core.show_2(_x1.fst, undefined) + ("pt," + ($std_core.show_2(_x1.snd, undefined) + ("pt," + ($std_core.show_2(bboxAdj, undefined) + ("pt," + ($std_core.show_1(_x1.thd) + ("," + ($common.showMime(render) + _x3)))))))))));
    }
  }
  var plainDims = $storage.readTextFileDef($std_path.changeExt(texNamePlain, ".dim"), "", undefined);
  var fullDims = $storage.readTextFileDef($std_path.changeExt(texNameFull, ".dim"), "", undefined);
  var dimsx = $std_core._plus__3($std_core.map_3($std_core.list_4($std_core.lines(plainDims)), function(line0  /* string */ ) {  return dimLine($options.getMathRender_1($options.math(opts)), $common.Plain, texNamePlain, plainPages, plainPageAdj, plainBboxAdj, line0);}), $std_core.map_3($std_core.list_4($std_core.lines(fullDims)), function(line1  /* string */ ) {  return dimLine($options.getMathRenderFull($options.math(opts)), $common.Full, texNameFull, fullPages, fullPageAdj, fullBboxAdj, line1);}));
  var txt = $std_core.join_4(dimsx, "\n");
  $storage.tryWriteTextFile(dimxFile, txt);
  var _x0 = $options.verbose(opts) >= 2;
  if (_x0) {
    var totalCount = ((((imageCount).value) + ((embedCount).value))|0);
    var totalSize = ((((imageSize).value) + ((embedSize).value))|0);
    $options.print(opts, (" math images  : " + ($std_core.align($std_core.show_1(((imageCount).value)), 4, undefined) + (" in " + $mathParse.showSize(((imageSize).value))))), 2);
    $options.print(opts, (" math embedded: " + ($std_core.align($std_core.show_1(((embedCount).value)), 4, undefined) + (" in " + $mathParse.showSize(((embedSize).value))))), 2);
    $options.print(opts, (" math total   : " + ($std_core.align($std_core.show_1(totalCount), 4, undefined) + (" in " + $mathParse.showSize(totalSize)))), 2);
  }
  else {
    $std_core._unit_;
  }
  return txt;
}
function isFulllatex(path)  /* (path : string) -> bool */  {
  return ($std_path.stemname(path) === "pdflatex");
}
function isLualatex(path)  /* (path : string) -> bool */  {
  return ($std_path.stemname(path) === "lualatex");
}
function isPlainLatex(path)  /* (path : string) -> bool */  {
  return ($std_path.stemname(path) === "latex");
}
function isXelatex(path)  /* (path : string) -> bool */  {
  return ($std_path.stemname(path) === "xelatex");
}
function pagesShow(pages)  /* (pages : mathStatic/pages) -> string */  {
  return $std_core.join_4($std_core.map_3(pages, function(rng  /* (int, int) */ ) {  if (rng.fst >= rng.snd) {  var _x0 = "";} else {  var _x0 = ("-" + $std_core.show_1(rng.snd));} return ($std_core.show_1(rng.fst) + _x0);}), ",");
}
var rxPSWarning = $std_regex.regex("^(warning: .*\\r\\n)+$", true, undefined);
function mathCmdToImg(cmd, dir, mode, pages, xopts, $continue)  /* (cmd : string, dir : string, mode : common/mathkind, pages : mathStatic/pages, xopts : options/options, continue : (int) -> io ()) -> io () */  {
  if ($std_core.isNil(pages)) {
    return $continue(0);
  }
  else {
    $std_core._unit_;
  }
  $options.print(xopts, ("generating math images...  (" + ($std_core.fill($common.show_3(mode), 5, undefined) + (") (" + (pagesShow(pages) + ")")))), undefined);
  $options.print(xopts, ("> " + cmd), 3);
  return $$process.system(cmd, function(err2  /* int */ , stdout2  /* string */ , stderr2  /* string */ ) {  var output = (stdout2 + stderr2); if (err2 !== 0) {  $options.printErr(xopts, ("> " + cmd), undefined); $options.printErr(xopts, ("error: failure while typesetting math: \n" + output), undefined); (((output).indexOf("Invalid Parameter -") >= 0)) ? $options.printErr(xopts, "hint: perhaps you forgot to install ImageMagick?\n      (http://www.imagemagick.org/script/binary-releases.php)", undefined) : $std_core._unit_; var _x0 = ($std_regex.contains(output, rxPSWarning, undefined)) ? 0 : err2; return $continue(_x0);} else {  var _x1 = $options.verbose(xopts) >= 4; (_x1) ? $options.print(xopts, output, undefined) : $std_core._unit_; return $continue(0);}}, $options.processTimeout(xopts), dir, undefined);
}
function mathConvImgCmd(mode, fname, mrender, opts, pages)  /* (mode : common/mathkind, fname : string, mrender : common/mathrender, opts : options/mathoptions, pages : mathStatic/pages) -> (string, int, double) */  {
  var baseImg = $std_path.combine($options.imgDir(opts), $std_path.stemname(fname));
  if (mrender === 2) {
    var fuzz = 0.2;
    var _x1 = (($options.svgFontFormat(opts) === "") || ($options.svgFontFormat(opts) === "none"));
    if (_x1) {
      var _x0 = " -n";
    }
    else {
      var _x2 = ($options.svgFontFormat(opts) === "svg");
      if (_x2) {
        var _x0 = "";
      }
      else {
        var _x0 = (" --font-format=" + $$process.quote($options.svgFontFormat(opts)));
      }
    }
    if ((fuzz > 0.0)) {
      var _x3 = (" -b" + ($std_core.showFixed(fuzz, 1) + "pt"));
    }
    else {
      var _x3 = "";
    }
    var cmd = ($$process.quote($options.dvisvg(opts)) + (_x0 + (_x3 + (" -e -j -v3 -d" + ($std_core.show_1($options.svgPrec(opts)) + (" " + (dvisvgPages(pages) + (" -o " + ($$process.quote((baseImg + "-%1p.svg")) + (" " + $$process.quote($std_path.basename(fname))))))))))));
    return $std_core._tuple3_(cmd, 0, fuzz);
  }
  if (mrender === 1) {
    if (($std_path.extname(fname) === ".dvi")){
      var cmd0 = ($$process.quote($options.dvipng(opts)) + (" -T tight -z9 -bg Transparent" + (" -D" + ($std_core.show_1($options.dpi(opts)) + (" " + (dvipngPages(pages) + (" -o " + ($$process.quote((baseImg + "-%d.png")) + (" " + $$process.quote($std_path.stemname(fname)))))))))));
      return $std_core._tuple3_(cmd0, 0, 0.0);
    }
  }
  var cmd1 = ($$process.quote($options.convert(opts)) + (" -trim -density " + ($std_core.show_1($options.dpi(opts)) + (" " + ($$process.quote(($std_path.basename(fname) + convertPages(pages))) + (" " + $$process.quote((baseImg + (convertSuffix(pages) + ".png")))))))));
  return $std_core._tuple3_(cmd1, $std_core._tilde_(1), 0.0);
}
function mathImageGC(newDim, oldDim, outDir)  /* (newDim : std/dict/dict<common/mathinfo>, oldDim : std/dict/dict<common/mathinfo>, outDir : string) -> io () */  {
  return $std_core.foreach($std_dict.list_1(oldDim), function(kv  /* (string, common/mathinfo) */ ) {  var _x0 = $std_dict._lb__rb__2(newDim, kv.fst); if (_x0 != null) {  return $std_core._unit_;} else {  var imageFile = $std_path.combine(outDir, $common.imageName(kv.snd)); return $storage.tryUnlinkSync(imageFile);}});
}
function mathDimAnalyse(dimxName, xopts, plainPages, fullPages, texNamePlain, texNameFull, plainPageAdj, plainBboxAdj, fullPageAdj, fullBboxAdj, $continue)  /* (dimxName : string, xopts : options/options, plainPages : mathStatic/pages, fullPages : mathStatic/pages, texNamePlain : string, texNameFull : string, plainPageAdj : int, plainBboxAdj : double, fullPageAdj : int, fullBboxAdj : double, continue : (maybe<(std/dict/dict<common/mathinfo>, string)>) -> io ()) -> io () */  {
  $options.print(xopts, "analyse and embed math images.", undefined);
  var dims2 = extendDim(plainPages, fullPages, texNamePlain, texNameFull, plainPageAdj, plainBboxAdj, fullPageAdj, fullBboxAdj, dimxName, xopts);
  var _x0 = $mathParse.parseMathDim(dims2, $options.math(xopts));
  ($std_core.bool_2(_x0.thd)) ? $options.print(xopts, _x0.thd, undefined) : $std_core._unit_;
  mathImageGC(_x0.fst, $options.dim($options.math(xopts)), $std_path.dirname(dimxName));
  return $continue($std_core.Just($std_core._tuple2_(_x0.fst, _x0.snd)));
}
function texToDvi(texcmd)  /* (texcmd : string) -> (string, string) */  {
  if (isXelatex(texcmd)) {
    return $std_core._tuple2_(" --no-pdf", ".xdv");
  }
  else {
    if (isFulllatex(texcmd)) {
      return $std_core._tuple2_(" --output-format=dvi", ".dvi");
    }
    else {
      return (isLualatex(texcmd)) ? $std_core._tuple2_(" --output-format=dvi", ".dvi") : $std_core._tuple2_("", ".dvi");
    }
  }
}
function mathSnippetsRender(mode, mrender, texcmd, srcName, texName, xopts, content, oldMath, pages, $continue)  /* (mode : common/mathkind, mrender : common/mathrender, texcmd : string, srcName : string, texName : string, xopts : options/options, content : string, oldMath : string, pages : mathStatic/pages, continue : (string) -> io ()) -> io () */  {
  function rendermsg(ext)  /* (ext : string) -> console () */  {
    return $options.print(xopts, ("running " + ($std_core.fill(($std_path.stemname(texcmd) + " on math..."), 19, undefined) + ("(" + ($std_core.fill($common.show_3(mode), 5, undefined) + (" -> " + (((ext).substr(1)) + (" -> " + ($common.show_2(mrender) + ")")))))))), undefined);
  }
  if ($common.isSvg(mrender)) {
    var _x0 = texToDvi(texcmd);
    if ($std_core.isNil(pages)) {
      return $continue(_x0.snd);
    }
    else {
      $std_core._unit_;
    }
    rendermsg(_x0.snd);
    return $runLatex.runLaTeX(srcName, texName, texcmd, _x0.fst, xopts, content, "", 0, false, function(err  /* int */ ) {  var _x1 = (err !== 0) ? "" : _x0.snd; return $continue(_x1);});
  }
  else {
    var _x2 = !(($common.isFull(mode) && isPlainLatex(texcmd)));
    if (_x2) {
      var pdfext = (isPlainLatex(texcmd)) ? ".dvi" : ".pdf";
      if ($std_core.isNil(pages)) {
        return $continue(pdfext);
      }
      else {
        $std_core._unit_;
      }
      rendermsg(pdfext);
      return $runLatex.runLaTeX(srcName, texName, texcmd, "", xopts, content, "", 0, false, function(err0  /* int */ ) {  var _x3 = (err0 !== 0) ? "" : pdfext; return $continue(_x3);});
    }
    else {
      if ($std_core.isNil(pages)) {
        return $continue(".pdf");
      }
      else {
        $std_core._unit_;
      }
      var notfound = "set either the \'Math Mode: Mathjax\' or the \'Latex: <cmd>\' key in the metadata";
      var _x4 = texToDvi(texcmd);
      rendermsg((_x4.snd + " -> ps -> pdf"));
      return $runLatex.runLaTeX(srcName, texName, texcmd, _x4.fst, xopts, content, notfound, 0, false, function(err1  /* int */ ) {  (err1 !== 0) ? $continue("") : $std_core._unit_; var dir = $std_path.dirname(texName); var stem = $std_path.stemname(texName); var dvipsCmd = ($$process.quote($options.dvips($options.math(xopts))) + (" -Ppdf -G0 " + $$process.quote((stem + ".dvi")))); var ps2pdfCmd = ($$process.quote($options.ps2pdf($options.math(xopts))) + (" " + $$process.quote((stem + ".ps")))); $options.print(xopts, "generating pdf from dvi...", undefined); $options.print(xopts, (">" + dvipsCmd), 3); return $$process.system(dvipsCmd, function(err2  /* int */ , stdout  /* string */ , stderr  /* string */ ) {  if (err2 !== 0) {  $options.printErr(xopts, ("> " + dvipsCmd), undefined); $options.printErr(xopts, ("error: failure while typesetting math: \n" + (stdout + stderr)), undefined); return $continue("");} else {  $options.print(xopts, (">" + ps2pdfCmd), 3); return $$process.system(ps2pdfCmd, function(err20  /* int */ , stdout2  /* string */ , stderr2  /* string */ ) {  if (err20 !== 0) {  $options.printErr(xopts, ("> " + ps2pdfCmd), undefined); $options.printErr(xopts, ("error: failure while typesetting math: \n" + (stdout2 + stderr2)), undefined); return $continue("");} else {  return $continue(".pdf");}}, $options.processTimeout(xopts), dir, undefined);}}, $options.processTimeout(xopts), dir, undefined);});
    }
  }
}
function mathToImg(namePlain, nameFull, xopts, plainPages, fullPages, $continue)  /* (namePlain : string, nameFull : string, xopts : options/options, plainPages : mathStatic/pages, fullPages : mathStatic/pages, continue : (int, double, int, double) -> io ()) -> io () */  {
  var _x0 = mathConvImgCmd($common.Plain, namePlain, $options.getMathRender_1($options.math(xopts)), $options.math(xopts), plainPages);
  var _x1 = mathConvImgCmd($common.Full, nameFull, $options.getMathRenderFull($options.math(xopts)), $options.math(xopts), fullPages);
  var outDir = $std_path.combine($std_path.dirname(namePlain), $options.imgDir($options.math(xopts)));
  var _x2 = !($storage.fexistsSync(outDir));
  if (_x2) {
    $options.print(xopts, ("create image directory: " + outDir), undefined);
    $storage.mkdirp(outDir, undefined);
  }
  else {
    $std_core._unit_;
  }
  return mathCmdToImg(_x0.fst, $std_path.dirname(namePlain), $common.Plain, plainPages, xopts, function(err1  /* int */ ) {  return mathCmdToImg(_x1.fst, $std_path.dirname(nameFull), $common.Full, fullPages, xopts, function(err2  /* int */ ) {  var _x2 = (err1 === 0 && err2 === 0); return (_x2) ? $continue(_x0.snd, _x0.thd, _x1.snd, _x1.thd) : $std_core._unit_;});});
}
function runMathStatic(content, inName, outName, texNamePlain, texNameFull, plainPages, fullPages, oldMathPlain, oldMathFull, opts0, $continue)  /* (content : string, inName : string, outName : string, texNamePlain : string, texNameFull : string, plainPages : mathStatic/pages, fullPages : mathStatic/pages, oldMathPlain : string, oldMathFull : string, opts0 : options/options, continue : (maybe<(std/dict/dict<common/mathinfo>, string)>) -> io ()) -> io () */  {
  return checkDviSvgVersion(opts0, texNamePlain, texNameFull, function(xopts  /* options/options */ ) {  return mathSnippetsRender($common.Plain, $options.getMathRender_1($options.math(xopts)), $options.getMathLatex(xopts), inName, texNamePlain, xopts, content, oldMathPlain, plainPages, function(extplain  /* string */ ) {  if ($std_core.isEmpty(extplain)) {  return $continue($std_core.Nothing);} else {  $std_core._unit_;} return mathSnippetsRender($common.Full, $options.getMathRenderFull($options.math(xopts)), $options.getMathLatexFull(xopts), inName, texNameFull, xopts, content, oldMathFull, fullPages, function(extfull  /* string */ ) {  if ($std_core.isEmpty(extfull)) {  return $continue($std_core.Nothing);} else {  $std_core._unit_;} return mathToImg($std_path.changeExt(texNamePlain, extplain), $std_path.changeExt(texNameFull, extfull), xopts, plainPages, fullPages, function(plainPageAdj  /* int */ , plainBboxAdj  /* double */ , fullPageAdj  /* int */ , fullBboxAdj  /* double */ ) {  return mathDimAnalyse($std_path.changeExt(outName, ".dimx"), xopts, plainPages, fullPages, texNamePlain, texNameFull, plainPageAdj, plainBboxAdj, fullPageAdj, fullBboxAdj, function(mbmath  /* maybe<(std/dict/dict<common/mathinfo>, string)> */ ) {  if ($std_core.isJust(mbmath)) {  var texNameFinalPlain = $std_path.changeExt(texNamePlain, ".final.tex"); var texNameFinalFull = $std_path.changeExt(texNameFull, ".final.tex"); $storage.tryRename(texNamePlain, texNameFinalPlain); $storage.tryRename(texNameFull, texNameFinalFull);} else {  $std_core._unit_;} return $continue(mbmath);});});});});});
}
 
// koka exports:
return { 'analyzeImagePng': analyzeImagePng, 'rxSvgId': rxSvgId, 'encodeGlyphIds': encodeGlyphIds, 'rxSvgHeight': rxSvgHeight, 'rxSvgWidth': rxSvgWidth, 'rxSvgGroupId': rxSvgGroupId, 'rxSvgTextStyle': rxSvgTextStyle, 'scopeStyles': scopeStyles, 'rxComment': rxComment, 'rxNl': rxNl, 'rxSpaces': rxSpaces, 'svgCompress': svgCompress, 'svgExtractDim': svgExtractDim, 'analyzeImageSvg': analyzeImageSvg, 'fileRenderPng': fileRenderPng, 'rxVersion': rxVersion, 'checkDviSvgVersion': checkDviSvgVersion, 'concurrent': concurrent, 'convertPages': convertPages, 'convertSuffix': convertSuffix, 'dvipngPages': dvipngPages, 'dvisvgPages': dvisvgPages, 'extendDim': extendDim, 'isFulllatex': isFulllatex, 'isLualatex': isLualatex, 'isPlainLatex': isPlainLatex, 'isXelatex': isXelatex, 'pagesShow': pagesShow, 'rxPSWarning': rxPSWarning, 'mathCmdToImg': mathCmdToImg, 'mathConvImgCmd': mathConvImgCmd, 'mathImageGC': mathImageGC, 'mathDimAnalyse': mathDimAnalyse, 'texToDvi': texToDvi, 'mathSnippetsRender': mathSnippetsRender, 'mathToImg': mathToImg, 'runMathStatic': runMathStatic };
});