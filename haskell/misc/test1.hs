{-# LANGUAGE BangPatterns #-}
-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/advanced/evaluation-order-and-state-tokens
-- https://www.seas.upenn.edu/~cis194/spring15/lectures/12-unsafe.html


import System.IO.Unsafe

helper2 i = print i >> return i

main2 = do
    one <- helper2 1
    two <- helper2 2
    print $ one + two


add !x y = x + y

helper i = unsafePerformIO $ print i >> return i

main_seq = do
    let one = helper 1
        two = helper 2
    print $ one `seq` one + two


main = do
  let one = helper 1
      two = helper 2
  print $ one + two

main_add = do
    let one = helper 1
        two = helper 2
    print $ add one two
