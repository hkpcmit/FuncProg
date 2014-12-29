mp2 :: Int -> Int -> Int
mp2 _ 0 = 1
mp2 m n = m * mp2 m (n-1)

mp5 :: Int -> Int -> Int
mp5 _ 0 = 1
mp5 m n = m * m `mp5` (n-1)

