module BinaryTree where

-- Algebraic type for a binary search tree of integers.  A tree may be empty, or it 
-- may have a root, a left subtree and a right subtree.  (Note that one or both of the
-- subtrees might be empty.)  

data Tree = Empty | MakeTree Tree Int Tree

-- Creates a multi-line sideways representation of a tree.  The root will be on the left,
-- with its left child below it and its right child above it.  If you tilt your head to the
-- left you'll see the structure of the tree.
-- The second parameter is the indentation level.
treeToString :: Tree -> Int -> String
treeToString Empty _ = ""
treeToString (MakeTree left root right) indent =
    rightString ++ 
    (spaceString indent) ++ (show root) ++ "\n" ++ 
    leftString
    where
    leftString = treeToString left (indent+4)
    rightString = treeToString right (indent+4)
    
-- Creates a string consisting of n spaces (assuming n >= 0)
spaceString :: Int -> String
spaceString 0 = ""
spaceString n = ' ':(spaceString (n-1))

-- treeToString will be used to display trees
instance Show Tree where show tree = treeToString tree 0
            
-- Measures the height of the tree
-- Takes greater of left subtree vs right subtree
height :: Tree -> Int
height Empty = 0
height (MakeTree left _ right) = 1 + (max (height left) (height right))

-- Searches for a value in the tree
search :: Int -> Tree -> Bool
search _ Empty = False
search x (MakeTree left val right) 
    | x == val = True
    | x > val = search x right
    | otherwise = search x left

-- Gets the largest value in the tree
maxValue :: Tree -> Int
maxValue Empty = error "Program error: no max value in empty tree"
maxValue (MakeTree _ val right) = maxRight val right
-- maxRight checks the right nodes until it reaches the 
-- furthest right - which is the largest value in the tree
    where
        maxRight maxVal Empty = maxVal
        maxRight maxVal (MakeTree _ val right) = maxRight val right

-- Adds a value into a copy of the tree
add :: Int -> Tree -> Tree
add x Empty = MakeTree Empty x Empty
add x (MakeTree left val right) 
    | x == val = (MakeTree left val right)
    | x > val = MakeTree left val (add x right)
    | otherwise = MakeTree (add x left) val right

-- Creates a tree from a list of values
createTree :: [Int] -> Tree
createTree lis = addToTree (reverse lis) 
-- Auxiliary function for create tree, adds the list using foldr
    where
        addToTree = foldr (\x tree -> add x tree) Empty 

-- Parses the Tree into a list in increasing order
treeToList :: Tree -> [Int]
treeToList Empty = []
treeToList tree = treeList tree []
    where
        treeList Empty xs = xs
        treeList (MakeTree Empty x Empty) xs = x:xs
        treeList (MakeTree left x right) xs = treeList left (x:treeList right xs)

-- Deletes a node in a copy of a tree
delete :: Int -> Tree -> Tree
delete x Empty = Empty
delete x (MakeTree left val right) 
    | x > val = MakeTree left val (delete x right)
    | x < val = MakeTree (delete x left) val right
    | x == val = removeNode (MakeTree left val right)
    | otherwise = (MakeTree left val right)

-- Auxiliary function for delete, removes the node 
-- and replaces it once found
removeNode :: Tree -> Tree
removeNode (MakeTree Empty x Empty) = Empty
removeNode (MakeTree Empty x right) = right
removeNode (MakeTree left x Empty) = left
removeNode (MakeTree left x right) = MakeTree newLeft newNode right
    where 
        newNode = maxValue left
        newLeft = delete newNode left
 














