module BST where


-- | Integer-labeled binary trees.
data Tree
   = Node Int Tree Tree   -- ^ Internal nodes
   | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree. This one satisfies the BST property.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))

-- | Some more trees that violate the BST property.
t3, t4, t5, t6 :: Tree
t3 = Node 3 (Node 2 (Leaf 1) (Leaf 4)) (Leaf 5)
t4 = Node 3 (Leaf 1) (Node 4 (Leaf 2) (Leaf 5))
t5 = Node 4 (Node 2 (Leaf 3) (Leaf 1)) (Leaf 5)
t6 = Node 2 (Leaf 1) (Node 4 (Leaf 5) (Leaf 3))

-- | All of the example trees in one list.
ts :: [Tree]
ts = [t1,t2,t3,t4,t5,t6]


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> map leftmost ts
--   [4,1,1,1,3,1]
--
leftmost :: Tree -> Int
leftmost (Leaf x) = x
leftmost (Node _ y _) = leftmost y


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> map rightmost ts
--   [9,9,5,5,5,3]
--
rightmost :: Tree -> Int
rightmost (Leaf x) = x
rightmost (Node _ _ z) = rightmost z


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> map maxInt ts
--   [9,9,5,5,5,5]
--
maxInt :: Tree -> Int
maxInt x = maximum (preorder x)




-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> map minInt ts
--   [1,1,1,1,1,1]
--
minInt :: Tree -> Int
minInt x = minimum (preorder x)


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
--   >>> map sumInts ts
--   [45,45,15,15,15,15]
--
sumInts :: Tree -> Int
sumInts x = sum (preorder x)



-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--   
--   >>> map preorder [t3,t4,t5,t6]
--   [[3,2,1,4,5],[3,1,4,2,5],[4,2,3,1,5],[2,1,4,5,3]]
--   
preorder :: Tree -> [Int]
preorder (Leaf x) = [x]
preorder (Node x y z) = x : preorder y ++ preorder z


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   
--   >>> map inorder [t3,t4,t5,t6]
--   [[1,2,4,3,5],[1,3,2,4,5],[3,2,1,4,5],[1,2,5,4,3]]
--   
inorder :: Tree -> [Int]
inorder (Leaf x) = [x]
inorder (Node x y z) = inorder y ++ x: inorder z


-- | Check whether a list is sorted in ascending order
--
--   >>> isSorted ([3])
--   True
--
--   >>> isSorted ([6,5,7])
--   False
--   
--   >>> map isSorted(map inorder ts)
--   [False,True,False,False,False,False]
--   
isSorted :: [Int] -> Bool
isSorted [x] = True
isSorted (x:y:list) | x > y = False | otherwise = isSorted (y:list)


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   
--   >>> map isBST ts
--   [False,True,False,False,False,False]
--   
isBST :: Tree -> Bool 
isBST x = isSorted(inorder x)


-- | Check whether a number is contained in a binary search tree.
--   You should assume that the given tree is a binary search tree
--   and *not* explore branches that cannot contain the value if
--   this assumption holds. The last two test cases violate the
--   assumption, but are there to help ensure that you do not
--   explore irrelevant branches.
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
--   >>> inBST 4 t3
--   False
--
--   >>> inBST 2 t4
--   False
--   
inBST :: Int -> Tree -> Bool 
inBST x y = isBST y && x `elem` preorder y