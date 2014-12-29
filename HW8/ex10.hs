foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f a [] = a
foldRight f a (x : xs) = f x (foldRight f a xs)

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a [] = return a
foldRightM f a (x : xs)
  = foldRightM f a xs >>= \y -> f x y

test :: String -> String -> IO String
test x y
  = do z <- getLine
       return (z ++ x ++ ", " ++ y)

