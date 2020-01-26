module LibLearn
    ( someFunc
    ) where

import Data.Monoid;

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someFunc2 :: IO ()
someFunc2 = someFunc

a = mconcat [Sum 5, Sum 6, Sum 10]

c :: Int -> Int
c a = 1 + a

b = 2

-- (eglot 'haskell-mode)
-- (require 'eglot) (eglot-ensure)
-- (require 'intero)
-- (intero-mode)
