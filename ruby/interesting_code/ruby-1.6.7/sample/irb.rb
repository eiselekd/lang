#!/usr/bin/env ruby
#
#   irb.rb - intaractive ruby
#   	$Release Version: 0.7.3 $
#   	$Revision: 1.2.2.1 $
#   	$Date: 2001/04/30 18:39:35 $
#   	by Keiju ISHITSUKA(keiju@ishitsuka.com)
#

require "irb"

if __FILE__ == $0
  IRB.start(__FILE__)
else
  # check -e option
  if /^-e$/ =~ $0
    IRB.start(__FILE__)
  else
    IRB.initialize(__FILE__)
  end
end
