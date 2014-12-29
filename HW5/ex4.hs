mand1 :: [Bool] -> Bool
mand1 [] = True
mand1 (b : bs) = b && mand1 bs

mand2 :: [Bool] -> Bool
mand2 [] = True
mand2 (b : bs)
  | b = mand2 bs
  | otherwise = False

mand5 :: [Bool] -> Bool
mand5 [] = True
mand5 (b : bs)
  | b ==False = False
  | otherwise = mand5 bs

mand6 :: [Bool] -> Bool
mand6 [] = True
mand6 (b : bs) = b || mand6 bs

mand7 :: [Bool] -> Bool
mand7 [] = True
mand7 (b : bs) = mand7 bs && b

mand8 :: [Bool] -> Bool
mand8 [] = True
mand8 (b : bs)
  | b = b
  | otherwise = mand8 bs




