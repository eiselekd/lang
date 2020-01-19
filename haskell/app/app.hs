{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE InstanceSigs #-}

-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#fmap
-- https://wiki.haskell.org/Evaluation_order_and_state_tokens

data Maybe2 v = Just2 v | Nothing2 deriving (Show)

instance Functor Maybe2 where

  fmap g (Just2 a) = Just2 (g a)
  fmap _ Nothing2  = Nothing2

instance  Applicative Maybe2 where

  pure x = Just2 x
  (<*>) :: Maybe2 (a->b) -> Maybe2 a -> Maybe2 b
  a <*> b =
    case a of
      Just2 a0 -> fmap a0 b
      Nothing2 -> Nothing2

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where

  fmap f g = ZipList (map f (getZipList g))

instance Applicative ZipList where

  pure :: a -> ZipList a
  pure = undefined

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

main :: IO ()
main = do

  print ( pure (+) <*> Just2 1 <*> Just2 2 )

  -- instance Show Maybe2 v where
  --   show Just2 a  = show a
  --   show Nothing2 = show ""

  -- (<*>) :: Maybe2 (a->b) -> Maybe2 a -> Maybe2 b
  -- Just2 f <*> m = fmap f m
  -- Nothing2 <*> _m = Nothing2

  -- print (pure (+) <*> Just2 1 <*> Just2 2)
  -- p <- (pure (+) <*> Just2 1 <*> Just2 2)
  -- case p of
  --   Just2 a  -> print "Hello"
  --   Nothing2 -> print "World"
  -- (pure (+) <*> (Just2 10) <*> (Just2 4))


-- newtype IO2 a = IO2 (State# RealWorld -> (# State# RealWorld, a #))
