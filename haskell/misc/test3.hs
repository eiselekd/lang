instance Applicative Myabe where
  pure a :: Maybe a
  (<*>) :: Maybe (a->b) -> Maybe a -> Maybe b
