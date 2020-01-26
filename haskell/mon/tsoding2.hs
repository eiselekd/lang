import Perform

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

(>>>=) :: WorldT a     --    World -> (a, World)
  -> (a -> WorldT b)   -- -> (a -> World -> (b, World))    ;; uncurry: (a,World) -> (b, World)
  -> WorldT b          -- -> World -> (b, World)
wt >>>= f = uncurry f  . wt


{-

/* ----- curry ------- */
function a(a0,a1) {
  return a0 + a1;
}
a(10,20);

function curry_a(a0) {
  return function (a1) {
    return a0 + a1;
  }
}
curry_a(10)(20);

function uncurry_curry_a(f) {
   return function (a0,a1) {
      f(a0)(a1);
   }
}


-}
