module HalfAdd
  (
    module Lava.Signal
 , module Lava.Generic
  , module Lava.Operators
  , module Lava.Combinational
  , module Lava.Sequential
  , module Lava.SequentialConstructive
  , module Lava.ConstructiveAnalysis
  , module Lava.Test
  , module Lava.Verification
  , module Lava.Vis
  , module Lava.Fixit
 , module Lava.Smv
  , module Lava.Satzoo
--  , module Lava.Minisat
  , module Lava.Property
  , module Lava.Retime
  , module Lava.Vhdl
  , halfAdd)
 where

import Lava
import Lava.Signal
import Lava.Generic
import Lava.Operators
import Lava.Combinational
import Lava.Sequential
import Lava.SequentialConstructive
import Lava.ConstructiveAnalysis
import Lava.Test
import Lava.Verification
import Lava.Vis
import Lava.Fixit
import Lava.Smv
import Lava.Satzoo
--import Lava.Minisat
import Lava.Property
import Lava.Retime
import Lava.Vhdl

halfAdd (a, b) = (sum, arry)
  where
    sum = xor2 (a, b)
    arry = and2 (a, b)

fullAdd (arryIn, (a, b)) = (sum, arryOut)
  where
    (sum1, arry1) = halfAdd (a, b)
    (sum, arry2) = halfAdd (arryIn, sum1)
    arryOut = xor2 (arry2, arry1)







prop_HalfAddOutputNeverBothTrue (a, b) = ok
  where
    (sum, arry) = halfAdd (a, b)
    --ok = sum <=> sum
    ok = nand2 (sum, arry)

-- genvar=var "v"
