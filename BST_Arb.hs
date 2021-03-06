module BST_Arb where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
encodeList :: [a] -> Tree a
encodeList [] = End
encodeList [x] = leaf x
encodeList (x:y:list) = Node x End (encodeList (y:list))


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ End = End
mapTree func (Node root left right) = Node (func root) (mapTree func left) (mapTree func right)




-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End = Nothing
valueAt [] (Node root _ _) = Just root
valueAt (x:list) (Node root left right) |
 x == L = valueAt list left |
 otherwise = valueAt list right


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo val tree |
 inList val (preorder tree) = Just (createPath val tree) |
 otherwise = Nothing


-- | Test preorder list creation
--   >>> preorder ex
--   [4,3,2,7,5,6,8]

preorder :: Tree a -> [a]
preorder End = []
preorder (Node root left right) = root : preorder left ++ preorder right

-- | Test value check in list
--   >>> inList 2 (preorder ex)
--   True
--   >>> inList 9 (preorder ex)
--   False

inList :: Eq a => a -> [a] -> Bool 
inList val list = val `elem` list


-- | Test path creation
--   >>> createPath 2 ex
--   [L,L]
--   >>> createPath 6 ex
--   [R,L,R]

createPath :: Eq a => a -> Tree a -> Path
createPath val (Node root left right) |
 root == val = [] |
 inList val (preorder left) = L : createPath val left |
 inList val (preorder right) = R : createPath val right
