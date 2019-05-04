module Main where
import           Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
main :: IO ()
main = do
  -- Using the IO monad:
  vec <-
    do v <- MV.new 1
       MV.write v 0 (1 :: Int)
       V.freeze v
  print vec
  -- Using the ST monad:
  print
    (runST
       (do v <- MV.new 1
           MV.write v 0 (1 :: Int)
           V.freeze v))
