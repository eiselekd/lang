Title         : Test
Sub Title     : Specification.
Heading Base  : 1
Heading Depth : 4
Toc Depth     : 4
Css           : https://fonts.googleapis.com/css?family=Noto+Sans:400,400italic,700,700italic
Css           : https://fonts.googleapis.com/css?family=Noto+Serif:400,400italic,700,700italic
Css           : https://fonts.googleapis.com/css?family=Roboto+Mono:400,500,700,400italic
Css           : https://fonts.googleapis.com/css?family=Roboto+Slab:300,400,700
Colorizer     : unchecked.json
Description   : Language Specification
[INCLUDE=book]
[INCLUDE=webtoc]
[INCLUDE=webanchors]

~bar          : before='|' 
~many         : before='{ ' after=' }'
~opt          : before='[ ' after=' ]'
xlapp          : &#x2987;
xlidx          : &#12310;
lapp          : _lapp_
lidx          : _lidx_

h1, h2, h3 {
  before: "[&#x1F517;](#&id;){.entity-anchor}"
}

toc.toc-contents {
  before:clear;
}

.pre-indented, .console {
  replace: "/^( *>.*)/\(**``\1``**\)/mg";
}

@if preview {
  .code1 {
    border-bottom: 1px solid green;
  }

  .pre-fenced3 {
    border-left: 0.5ex solid green; 
  }
  
  .token.predefined {
    color: navy;
  }
}

h4 {
  @h1-h2-h3-h4: upper-alpha;
}

body {
  font-family: 'Noto Serif','Cambria', "Times New Roman", "Liberation Serif", "Times", serif;
}

.toc, h1, h2, h3, h4, h5 {
  font-family: 'Noto Sans', 'Segoe UI', sans-serif;
}


Html Header   : 
  <!-- NO_CLICK_TRACKING -->
  <!--
    Copyright 2012-2016 Microsoft Corporation.
   
    This is free software; you can redistribute it and/or modify it under the
    terms of the Apache License, Version 2.0. A copy of the License can be
    found in the file "license.txt" at the root of this distribution.
  -->

[TITLE]

~ Begin SidePanel

[TOC]

~ End SidePanel

~ Begin MainPanel

[INCLUDE=test_p1.md]


[BIB]


~ End MainPanel


