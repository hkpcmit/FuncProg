putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])

getLine' :: IO String
getLine' = get []

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

interact' :: (String -> String) -> IO ()
interact' f
  = do input <- getLine'
       putStrLn' (f input)

sequence2_' :: Monad m => [m a] -> m ()
sequence2_' [] = return ()
sequence2_' (m : ms) = (foldl (>>) m ms) >> return ()
 
sequence4_' :: Monad m => [m a] -> m ()
sequence4_' [] = return ()
sequence4_' (m  :ms) = m >> sequence4_' ms
 
sequence5_' :: Monad m => [m a] -> m ()
sequence5_' [] = return ()
sequence5_' (m : ms) = m >>= \_ -> sequence5_' ms
 
sequence6_' :: Monad m => [m a] -> m ()
sequence6_' ms = foldr (>>) (return ()) ms

