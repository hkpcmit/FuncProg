isEqual :: String -> IO Bool
isEqual xs
  = do inp <- getLine
       return (xs == inp)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys


