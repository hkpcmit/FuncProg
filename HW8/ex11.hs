liftM1 :: Monad m => (a -> b) -> m a -> m b
liftM1 f m
  = do x <- m
       return (f x)

liftM3 :: Monad m => (a -> b) -> m a -> m b
liftM3 f m = m >>= \a -> return (f a)

test :: String -> String
test = \x -> "test: " ++ x
