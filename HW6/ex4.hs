mdw1 _ [] = []
mdw1 p (x : xs)
  | p x = mdw1 p xs
  | otherwise = x : xs

mdw4 p = foldl add []
  where add [] x = if p x then [] else [x]
        add acc x = x:acc
