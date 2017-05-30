module Tree where

data Tree a = Tree a [Tree a]
     deriving (Show)

makeTree :: t -> Tree t
makeTree x = Tree x []

--Extend the tip of every branch
grow :: (a -> [a]) -> Tree a -> Tree a
grow fn (Tree a []) = Tree a (map makeTree $ fn a)
grow fn (Tree a ts) = (Tree a (map (grow fn) ts))

iterateGrowth :: (a -> [a]) -> (Tree a) -> (Tree a)
iterateGrowth fn tree = iterateGrowth fn (grow fn tree)

flatten :: Tree a -> [a]
flatten (Tree v ts) = [v] ++ (concat $ map flatten ts)

--Given a function that maps v->t,
--turn it into a function that returns Tree v -> t
makePruner :: (t->Bool) -> (Tree t -> Bool)
makePruner fn = \(Tree t _) -> fn t

--Remove all trees that match the predicate,
--and all their subtrees without inspection
prune :: (a->Bool) -> Tree a -> Tree a
prune fn (Tree v ts) =
          Tree v (map (prune fn)
               (filter (makePruner fn) ts))
          
---Now some growth functions
extendBinary :: String -> [String]
extendBinary b = ['0':b, '1':b]

binaryTree = makeTree ""

b2 = (grow extendBinary) $ (grow extendBinary) $ grow extendBinary binaryTree
