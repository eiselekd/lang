{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{- http://dev.stephendiehl.com/fun/002_parsers.html -}

module NanoParsec where

import Data.Char()
import Control.Monad()
import Control.Applicative()

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, xs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."
