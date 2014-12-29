removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y: ys)
  | x == y = ys
  | otherwise = y: removeone x ys
