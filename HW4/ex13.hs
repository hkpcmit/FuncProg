divides :: Int -> Int -> Bool
divides n m = n `mod` m == 0

divisors :: Int -> [Int]
divisors n = [i | i <- [1..n], n `divides` i]
