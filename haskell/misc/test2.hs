{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.List
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Unsafe.Coerce

-- import qualified Data.ByteString as BS

-- https://www.seas.upenn.edu/~cis194/spring15/lectures/12-unsafe.html


-- newIORef :: a IO (IORef a)

-- Read from an IO Ref
-- readIORef   :: IORef a -> IO a

-- Write to an IO Ref
-- writeIORef  :: IORef a -> a -> IO ()

-- Update the value of an IO Ref
-- modifyIORef :: IORef a -> (a -> a) -> IO ()



-- count :: Eq a => [a] -> a -> IO Int
-- count xs x = do
--   r <- newIORef 0
--   forM_ xs $ \y ->
--     when (x == y) $
--       modifyIORef r (+1)
--   readIORef r

-- count3 :: Eq a => [a] -> a -> Int
-- count3 xs x = unsafePerformIO $ do
--   r <- newIORef 0
--   forM_ xs $ \y ->
--     when (x == y) $
--       modifyIORef' r (+1)
--   readIORef r


newCounter :: IO (IO Int)
newCounter = do
  r <- newIORef 0
  return $ do
    v <- readIORef r
    writeIORef r (v + 1)
    return v

printCounts :: IO ()
printCounts = do
  c <- newCounter
  print =<< c
  print =<< c
  print =<< c
