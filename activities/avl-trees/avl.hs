--Activity 04 - AVL Tree
--Eric Faurie

--Note: the beginning commented section is my failed attempt at the subject
  --After much time and failure I consulted the internet for a solution
  --I then used this solution (typed in myself), and broke it down to 
  --teach myself how it was done. Hopefully this is acceptable.

--data AVLTree a = Empty 
--               | Node a (AVLTree a) (AVLTree a) Int
--               deriving (Show, Read, Eq)

--singleton x = Node x EmptyTree EmptyTree 0

--add elt EmptyTree = singleton x
--add elt (Node x left right)
--  | elt == x = Node x left right
--  | elt < x = Node x (add elt left) right
--  | elt > x = Node x left (add elt right)

--find x EmptyTree = False
--find x (Node a left right)
--  | x == a = True
--  | x < a = find x left
--  | x > a = find x right

--rrotate (Node x left EmptyTree) =   
--rrotate (Node x left right) = 

--lrotate (Node x EmptyTree right) =
--lrotate (Node x left right) =

--END FAILURE--

--standard definition, the tree is either empty or contains a node
data AVLTree a = Empty
               | Br (AVLTree a) a (AVLTree a) Int
--Note: the node Br is made up of a left subtree, a key, right subtree
--and a balance

--the $ operator ensures ins t is evaluated first
--ins returns an ordered pair, and then fst takes the first element of it
insert::(Ord a) => AVLTree a -> a -> AVLTree a
insert t x = fst $ ins t where
    --if the tree param is empty, the tree is now a single node,
    --with key = x, balance = 0, and total tree height of 1
    ins Empty = (Br Empty x Empty 0, 1)
    --otherwise the tree is not empty and requires iteration
    ins (Br l k r d)
        --if less than the current key, search left subtree
        | x < k     = node (ins l) k (r, 0) d
        --if its the same, return the current node, no changes
        | x == k    = (Br l k r d, 0)
        --otherwise its greater, search right subtree
        | otherwise = node (l, 0) k (ins r) d

--defines a node, 
  --it is an element containing a left subtree with balance dl
  --a key value k, a right subtree with blance dr, and a node balance of d
node :: (AVLTree a, Int) -> a -> (AVLTree a, Int) -> Int -> (AVLTree a, Int)
node (l, dl) k (r, dr) d = balance (Br l k r d', delta) where
    --assigns a balance to the node
    d' = d + dr - dl
    --assigns a blance that required a rotation
    delta = deltaH d d' dl dr

--determines which rotation is required
deltaH :: Int -> Int -> Int -> Int -> Int
deltaH d d' dl dr
       | d >= 0 && d' >= 0 = dr
       | d <= 0 && d' >= 0 = d+dr
       | d >= 0 && d' <= 0 = dl - d
       | otherwise = dl

--handles the rotations
balance :: (AVLTree a, Int) -> (AVLTree a, Int)
--blance on left is larger, right rotation
balance (Br (Br ( Br a x b dx) y c (-1)) z d (-2), _) = 
    (Br (Br a x b dx) y (Br c z d 0) 0, 0)
--blance on right is larger, left rotation
balance (Br a x (Br b y (Br c z d dz) 1) 2, _) =
    (Br (Br a x b 0) y (Br c z d dz) 0, 0)
--double left rotation
balance (Br (Br a x (Br b y c dy) 1) z d (-2), _) = 
    (Br (Br a x b dx') y (Br c z d dz') 0 , 0) where
        dx' = if dy == 1 then -1 else 0
        dz' = if dy == -1 then 1 else 0
--double right rotation
balance (Br a x (Br (Br b y c dy) z d (-1)) 2, _) =
    (Br (Br a x b dx') y (Br c z d dz') 0, 0) where
        dx' = if dy == 1 then -1 else 0
        dz' = if dy == -1 then 1 else 0
--tree is balanced
balance (t, d) = (t, d)

--a helper function providing the height of the entire tree
height :: (AVLTree a) -> Int
height Empty = 0
height (Br l _ r _) = 1 + max (height l) (height r)

--helper function checks if the delta's are valid
checkDelta :: (AVLTree a) -> Bool
checkDelta Empty = True
checkDelta (Br l _ r d) = and [checkDelta l, checkDelta r, d ==
  (height r - height l)]

--a simple way to build the tree from a list with HOF
fromList :: (Ord a) => [a] -> AVLTree a
fromList = foldl insert Empty

--a simple way to describe the tree as a list
toList :: (AVLTree a) -> [a]
toList Empty = []
toList (Br l k r _) = toList l ++ [k] ++ toList r


