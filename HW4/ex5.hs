find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] ->[Int]
positions x xs =
  [i | (x', i) <- zip xs [0..n], x == x']
  where n = length xs - 1

ps1 :: (Eq a) => a -> [a] -> [Int]
ps1 x xs = find x (zip xs [0..n])
  where n = length xs - 1

--ps2 :: (Eq a) => a -> [a] -> [Int]
--ps2 x xs = find x xs

--ps3 :: (Eq a) => a -> [a] -> [Int]
--ps3 x xs = find x (zipWith (+) xs [0..n])
--  where n = length xs - 1

--ps4 :: (Eq a) => a -> [a] -> [Int]
--ps4 x xs = find n (zip xs [0..x])
--  where n = length xs - 1
