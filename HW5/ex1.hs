mlength :: [a] -> Int
mlength [] = 0
mlength (_: xs) = 1 + mlength xs
