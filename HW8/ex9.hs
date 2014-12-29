foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft f a [] = a
foldLeft f a (x : xs) = foldLeft f (f a x) xs

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x : xs)
  = f a x >>= \y -> (foldLeftM f y xs) 

test :: String -> String -> IO String
test x y
  = do z <- getLine
       return (z ++ x ++ ", " ++ y)

