f11 :: Int -> Int
f11 n = 1

f12 :: Int -> Int -> (Int -> Int)
f12 n m = f11

f1 :: Int -> Int -> Int -> Int -> Int
f1 n = f12
