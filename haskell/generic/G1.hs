
newtype V a = V a

class C0 a where
  var :: String -> V

test0 :: C0 a => (a -> b) -> b
test0 a = a (var "i")

--test1 :: Constructive1 a => (a -> b) -> b
--test1 a = a (var "i")
