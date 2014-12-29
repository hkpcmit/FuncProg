land1 :: Bool -> Bool -> Bool
land1 True True = True
land1 _ _ = False

land2 :: Bool -> Bool -> Bool
land2 a b = if a then if b then True else False else False

land3 :: Bool -> Bool -> Bool
land3 a b = if not (a) then not (b) else True

land5 :: Bool -> Bool -> Bool
land5 a b = if a then if b then False else True else False

land6 :: Bool -> Bool -> Bool
land6 a b = if a then b else False

land7 :: Bool -> Bool -> Bool
land7 a b = if b then a else False

