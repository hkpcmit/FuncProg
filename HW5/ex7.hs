mid :: [a] -> Int -> a
mid (x : _) 0 = x
mid (_ : xs) n = mid xs (n-1)

