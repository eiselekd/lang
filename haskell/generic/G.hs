{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

class BaseAdder a where
  addimpl :: a -> a -> a
instance BaseAdder Int where
  addimpl = (+)
instance BaseAdder [a] where
  addimpl = (++)

class Adder a where
  doadd :: a -> a -> a
instance BaseAdder a => Adder a where
  doadd = addimpl

ha (a,b) = doadd a b

test0 = ha ( 1 :: Int , 2 :: Int)
test1 = ha ( "a" , "b")

cmp a b = (==) a  b

test2 = cmp 1 2
test3 = cmp "a" "b"





-- class Constructive a where
--   var    :: String -> a

-- instance (Constructive a, Constructive b)
--       => Constructive (a, b) where
--   var s    = (var (s ++ "_1"), var (s ++ "_2"))


-- test0 :: (Constructive a, Generic b) => (a -> b) -> b
-- test0 =


-- run0 = test0 ha
