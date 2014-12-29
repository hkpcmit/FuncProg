test :: [[a]] -> [[a]]
test [] = []
test [x] = [x]
test [x:xs] = [x:xs]