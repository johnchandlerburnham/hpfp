--11/BinaryTree.hs
module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Show, Ord)
-- insert
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- map
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = (Node (mapTree f left) (f a) (mapTree f right))

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = mapTree (+1) testTree' == mapExpected

-- list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++  (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder = preorder testTree == [2, 1, 3]

testInorder = inorder testTree == [1, 2, 3]

testPostorder = postorder testTree == [1, 3, 2]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f ac Leaf = ac
foldTree f ac (Node left v right) = foldTree f (foldTree f (f v ac) left) right

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' f ac bt = foldr f ac (inorder bt)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree g Leaf bt
  where g v acc = Node acc (f v) Leaf

mapTree2 :: Ord b => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree2 f bt = foldTree g Leaf bt
  where g v acc = insert' (f v) acc

testTree2 = Node (Node (Leaf) 2 (Node Leaf 5 Leaf)) 1
                 (Node (Node Leaf 6 Leaf) 3 (Node Leaf 7 Leaf))

 
foldTree1 :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree1 f a Leaf = a
foldTree1 f a (Node left v right) = 
  f v (foldTree1 f a left) (foldTree1 f a right)

mapTree1 :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree1 f bt = foldTree1 g Leaf bt where
  g a left right = Node left (f a) right
