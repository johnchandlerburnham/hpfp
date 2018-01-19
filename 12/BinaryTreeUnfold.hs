--12/BinaryTreeUnfold.hs
module BinaryTreeUnfold where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Show, Ord)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f s = case f s of
  Nothing -> Leaf
  Just (l, v, r) -> Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0 where
  f n x = if x >= n then Nothing else Just (x+1, x, x+1) 
