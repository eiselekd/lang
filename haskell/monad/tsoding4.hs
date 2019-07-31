{-# LANGUAGE DeriveFunctor #-}

-- https://www.youtube.com/watch?v=fCoQb-zqYDI
import Perform

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

infixl 1 >>>=
(>>>=) :: WorldT a     --    World -> (a, World)
  -> (a -> WorldT b)   -- -> (a -> World -> (b, World))
  -> WorldT b          -- -> World -> (b, World)
wt >>>= f = uncurry f .wt
-- uncurry: (a -> World -> (b, World)) => ((a, World) -> (b,World))

whatIsYourPureNameT :: WorldT ()
whatIsYourPureNameT =
  printStrT "What is your name" >>>= \_ ->
  readStrT                      >>>= \n ->
  printStrT ("Hello " ++  n)

-------------------------
-- do notation

newtype WorldM a = WorldM { asT :: WorldT a } deriving Functor

instance Applicative WorldM where
  pure x = WorldM (\w ->(x,w))
  wtf <*> wt = WorldM (asT wtf >>>= \f ->
                       asT wt  >>>= \x ->
                       asT $ pure $ f x )

instance Monad WorldM where
  wt >>= f = WorldM (asT wt >>>= asT . f)

printStrM :: String -> WorldM ()
printStrM = WorldM . printStrT

readStrM :: WorldM String
readStrM = WorldM readStrT

whatIsYourPureNameM :: WorldM ()
whatIsYourPureNameM = do
  printStrM "What is your name?"
  name <- readStrM
  printStrM ("hello" ++ name)

-- ghci> asT whatIsYourPureNameM World
