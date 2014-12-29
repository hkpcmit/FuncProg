mcon :: [[a]] -> [a]
mcon [] = []
mcon (xs: xss) = xs ++ mcon xss
