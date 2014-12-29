mel :: Eq a => a -> [a] -> Bool
mel _ [] = False
mel x (y : ys)
  | x == y = True
  | otherwise = mel x ys

