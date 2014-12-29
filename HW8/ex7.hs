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

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)
 
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m  :ms) = m >> sequence_' ms

mapM1' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1' f as = sequence' (map f as)

mapM2' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2' f [] = return []
mapM2' f (a : as)
  = f a >>= \b -> mapM2' f as >>= \bs -> return (b : bs)

mapM6' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM6' f [] = return []
mapM6' f (a : as)
  = do b <- f a
       bs <- mapM6' f as
       return (b : bs)

mapM7' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM7' f [] = return []
mapM7' f (a : as)
  = f a >>=
      \b ->
        do bs <- mapM7' f as
           return (b : bs)
