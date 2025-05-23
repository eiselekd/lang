`line 2 "inc3_a_filename_from_line_directive" 0
// DESCRIPTION: Verilog::Preproc: Example source code
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2000-2005 by Wilson Snyder.

`ifndef _EXAMPLE_INC2_V_
 `define _EXAMPLE_INC2_V_ 1
 `define _EMPTY
  // FOO
  At file `__FILE__  line `__LINE__
`else
  `error "INC2 File already included once"
`endif // guard

`ifdef not_defined
 `include "NotToBeInced.v"
`endif
