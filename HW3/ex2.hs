lor1 :: Bool -> Bool -> Bool
lor1 False False = False
lor1 _ _ = True

lor2 :: Bool -> Bool -> Bool
lor2 False b = b
lor2 True _ = True

lor3 :: Bool -> Bool -> Bool
lor3 b c
  | b == c = True
  | otherwise = False

lor4 :: Bool -> Bool -> Bool
lor4 b c
  | b == c = b
  | otherwise = True

lor5 :: Bool -> Bool -> Bool
lor5 b False = b
lor5 _ True = True

lor6 :: Bool -> Bool -> Bool
lor6 b c
  | b == c = c
  | otherwise = True

lor7 :: Bool -> Bool -> Bool
lor7 b True = b
lor7 _ True = True

lor8 :: Bool -> Bool -> Bool
lor8 False False = False
lor8 False True = True
lor8 True False = True
lor8 True True = True
