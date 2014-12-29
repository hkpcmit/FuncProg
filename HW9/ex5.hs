data Tree = Leaf Integer
          | Node Tree Tree
          deriving Show

tree1 = Node (Leaf 1) (Leaf 3)
tree2 = Node (Leaf 101) (Leaf 103)
tree3 = Node (Leaf 101) (Node (Leaf 123) (Node (Leaf 143) (Leaf 165)))

leaves :: Tree -> Integer
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r)
  = abs (leaves l - leaves r) <= 1 && balanced l && balanced r
