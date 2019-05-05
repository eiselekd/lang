--Simple interpreter for an extensible lambda calculus
-- LCInterpreter.hs
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module LCInterpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr p v
  = Var v
  | LitVal p
  | Let v (Expr p v) (Expr p v)
  | Lam v (Expr p v)
  | App (Expr p v) (Expr p v)
  deriving (Show, Eq)

infixl 1 `App`

data Val p v
  = PrimVal p
  | Closure (Val p v -> EvalM p v (Val p v))

instance Show p => Show (Val p v) where
  show (PrimVal p) = show p
  show (Closure _) = "<closure>"

newtype EvalM p v a = EvalM { runEvalM :: Scope p v -> Either (EvalErr p v) a }
  deriving (Functor, Applicative, Monad, MonadReader (Scope p v), MonadError (EvalErr p v)) via ReaderT (Scope p v) (Either (EvalErr p v))

type Scope p v = Map v (Val p v)

data EvalErr p v = TypeErr String | UnboundVarErr v (Scope p v)
  deriving (Show)

eval :: Ord v => Expr p v -> EvalM p v (Val p v)
eval (Var v) = asks (Map.lookup v) >>= \case
  Nothing -> throwError . UnboundVarErr v =<< ask
  Just x -> pure x
eval (LitVal p) = pure (PrimVal p)
eval (Let v e b) = do
  x <- eval e
  local (Map.insert v x) (eval b)
eval (Lam v b) = pure $ Closure $ \x -> local (Map.insert v x) (eval b)
eval (App f e) = do
  body <- eval f >>= \case
    PrimVal _ -> throwError (TypeErr "not a function")
    Closure f' -> pure f'
  body =<< eval e

arith :: Scope Int String
arith = Map.fromList
  [ ("+", primFunc2 (+))
  , ("-", primFunc2 (-))
  , ("*", primFunc2 (*))
  , ("negate", primFunc negate)
  , ("abs", primFunc abs)
  , (".", compose)
  ]

compose :: Val p v
compose = Closure $ \case
  PrimVal _ -> throwError (typeErr "expected a function")
  Closure f -> Closure <$> \case
    PrimVal _ -> throwError (typeErr "expected a function")
    Closure g -> Closure (f <=< g)

primFunc :: (p -> p) -> Val p v
primFunc f = Closure $ \case
  PrimVal p -> pure (PrimVal (f p))
  Closure _ -> throwError (TypeErr "expected a value, got a function")

primFunc2 :: (p -> p -> p) -> Val p v
primFunc2 f = Closure $ \case
  PrimVal p -> pure $ primFunc (f p)
  Closure _ -> throwError (TypeErr "expected a value, got a function")

double :: Expr Int String
double = Lam "x" (Var "*" `App` Var "x" `App` LitVal 2)
