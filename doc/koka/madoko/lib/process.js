// koka generated module: "process"
if (typeof define !== 'function') { var define = require('amdefine')(module) }
define(['child_process', './std_core', './std_path', './std_dict'], function(child_process, $std_core, $std_path, $std_dict) {
"use strict";
 
// koka declarations:
/*---------------------------------------------------------------------------
  Copyright 2013-2015 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
 
// quote file and command names for shell execution
function quote(s)  /* (s : string) -> string */  {
  return ("\"" + (s + "\""));
}
function systemx(cmd, callback, timeout, cwd, env)  /* forall<e> (cmd : string, callback : (int, string, string) -> <io|e> (), timeout : ?int, cwd : ?string, env : ?maybe<std/dict/dict<string>>) -> <io|e> () */  {
  var timeout_94 = (timeout !== undefined) ? timeout : 0;
  var cwd_98 = (cwd !== undefined) ? cwd : "";
  var env_103 = (env !== undefined) ? env : $std_core.Nothing;
  return child_process.exec(cmd,{timeout:timeout_94,cwd:(cwd_98!=''?cwd_98:undefined),env:(env_103?(env_103).unJust:undefined),windowsVerbatimArguments:true},function(err,stdout,stderr) { (callback)(err?err.code:0,stdout,stderr); });;
}
function system(cmd, callback, timeout, cwd, env)  /* forall<e> (cmd : string, callback : (int, string, string) -> <io|e> (), timeout : ?int, cwd : ?string, env : ?list<(string, string)>) -> <io|e> () */  {
  var timeout_120 = (timeout !== undefined) ? timeout : 0;
  var cwd_124 = (cwd !== undefined) ? cwd : "";
  var env_129 = (env !== undefined) ? env : $std_core.Nil;
  if ($std_core.isNil(env_129)) {
    var _x0 = $std_core.Nothing;
  }
  else {
    var _x0 = $std_core.Just($std_dict.dict_1(env_129));
  }
  return systemx(cmd, callback, timeout_120, cwd_124, _x0);
}
 
// koka exports:
return { 'quote': quote, 'systemx': systemx, 'system': system };
});