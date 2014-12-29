{-# LANGUAGE NPlusKPatterns #-}

fff :: [a] -> a -> [a]
fff = \xs x -> x : xs

bbb :: [Char]
bbb = ['a'..'e'] ++ ['1'..'5']

ex0 = foldl fff [] bbb