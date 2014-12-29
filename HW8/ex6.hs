putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])

getLine' :: IO String
getLine' = get []

sequence1_' :: Monad m => [m a] -> m [a]
sequence1_' [] = return []
sequence1_' (m : ms)
  = m >>=
      \a ->
       do as <- sequence1_' ms
          return (a : as)

sequence5_' :: Monad m => [m a] -> m [a]
sequence5_' ms = foldr func (return []) ms
  where
        func :: (Monad m) => m a -> m [a] -> m [a]
        func m acc
          = do x <- m
               xs <- acc
               return (x : xs)

sequence8_' :: Monad m => [m a] -> m [a]
sequence8_' [] = return []
sequence8_' (m : ms)
  = do a <- m
       as <- sequence8_' ms
       return (a : as)
