-- https://www.youtube.com/watch?v=fCoQb-zqYDI

import Perform

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

(>>>=) :: WorldT a     --    World -> (a, World)
  -> (a -> WorldT b)   -- -> (a -> World -> (b, World)) ;; uncurry: (a,World) -> (b, World)
  -> WorldT b          -- -> World -> (b, World)
wt >>>= f = uncurry f .wt

whatIsYourPureNameT :: WorldT ()
whatIsYourPureNameT =
  printStrT "What is your name" >>>= \_ ->
  readStrT                      >>>= \n ->
  printStrT ("Hello " ++ n)

-- https://wiki.haskell.org/Evaluation_order_and_state_tokens
-- ~/src/ghc-8.0.2/libraries/base/GHC/
-- newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
-- instance  Monad IO  where
--     (>>=)     = bindIO
-- bindIO :: IO a -> (a -> IO b) -> IO b
-- bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s
