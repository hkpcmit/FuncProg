removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y: ys)
  | x == y = ys
  | otherwise = y: removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x: xs) [] = False
isChoice (x: xs) ys = elem x ys && isChoice xs (removeone x ys)

isChoice4 :: Eq a => [a] -> [a] -> Bool
isChoice4 [] _ = True
isChoice4 (x: xs) [] = False
isChoice4 (x: xs) ys = elem x ys && isChoice (removeone x xs) ys
