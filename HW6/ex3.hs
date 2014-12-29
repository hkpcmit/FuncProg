mtw _ [] = []
mtw p (x : xs)
  | p x = x : mtw p xs
  | otherwise = []