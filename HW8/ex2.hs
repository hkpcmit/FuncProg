putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn1' :: String -> IO ()
putStrLn1' [] = putChar '\n'
putStrLn1' xs = putStr' xs >> putStrLn1' ""

putStrLn2' :: String -> IO ()
putStrLn2' [] = putChar '\n'
putStrLn2' xs = putStr' xs >> putChar '\n'

putStrLn3' :: String -> IO ()
putStrLn3' [] = putChar '\n'
putStrLn3' xs = putStr' xs >>= \x -> putChar '\n'

putStrLn5' :: String -> IO ()
putStrLn5' [] = putChar '\n'
putStrLn5' xs = putStr' xs >> putStr' "\n"
