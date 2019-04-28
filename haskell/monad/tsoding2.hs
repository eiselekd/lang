import Perform

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

(>>>=) :: WorldT a     --    World -> (a, World)
  -> (a -> WorldT b)   -- -> (a -> World -> (b, World))
  -> WorldT b          -- -> World -> (b, World)
wt >>>= f = uncurry f .wt
