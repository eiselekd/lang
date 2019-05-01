module Lava.LavaDir where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

-- import Paths_chalmers_lava2000

getLavaDir :: IO FilePath
getLavaDir = catchIO (getEnv "chalmers_lava2000_datadir") (\_ -> return "/tmp/")
