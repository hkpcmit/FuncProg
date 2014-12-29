data Tree = Leaf Integer
          | Node Tree Integer Tree
          deriving Show

tree1 = Node (Leaf 1) 2 (Leaf 3)
tree2 = Node (Leaf 101) 102 (Leaf 103)

occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r)
  = case compare m n of
        LT -> occurs1 m l
        EQ -> True
        GT -> occurs1 m r

occurs5 :: Integer -> Tree -> Bool
occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r)
  | m == n = True
  | m < n = occurs5 m l
  | otherwise = occurs5 m r
