\begin{code}

-- helper functions:

-- add all values to a set
addAll :: Ord a => [a] -> Set a
addAll [] = Empty
addAll (x:xs) = if x `elem` xs -- no duplicates allowed!
					then addAll(xs)
				else
					add x (addAll (xs))  

-- breadth first tree traversal to obtain the values from the BST:
toArray :: Set a -> [a]
toArray Empty = []
toArray (Leaf v) = [v]
toArray (Node v left right) = (v : toArray left) ++ toArray right

-- an intersect method for two lists using a fold:
listIntersection :: Eq a =>  [a] -> [a] -> [a]
listIntersection xs ys = foldl (\acc x -> if x `elem` ys then x : acc else acc) [] xs

-- a method to determine if a list is a subset of a list, using a left fold:
listSubset :: Eq a => [a] -> [a] -> Bool 
listSubset xs ys = foldl (\acc x -> if not (x `elem` ys) then False else acc) True xs

-- select all values from a list that satisfy the given condition:
listSelect :: (a -> Bool) -> [a] -> [a]
listSelect cond xs = foldl (\acc x -> if cond x then x : acc else acc) [] xs


\end{code}

\begin{code}

-- Binary Search Tree Set:

data Set a = Empty -- nothing
			| Leaf a  -- a leaf with value a
			| Node a (Set a) (Set a) -- a node with value a, and two children  
			deriving (Show)

makeSet :: Ord a => [a] -> Set a
makeSet [] = Empty
makeSet values = addAll (reverse values) -- kinda hacky, but works for now 

add :: Ord a => a -> Set a -> Set a 
add a Empty = Leaf a 
add a (Leaf v) | a < v = Node v (Leaf a) Empty 
			   | a >= v = Node v Empty (Leaf a)

add a (Node v left right) | a < v = Node v (add a left) right 
						  | a >= v = Node v left (add a right)

has :: Ord a => a -> Set a -> Bool
has a Empty = False
has a (Leaf v) = (a == v)
has a (Node v left right) = if a == v
							then True
						else if (a < v) 
							then has a left
						  else
						    has a right

card :: Set a -> Int
card Empty = 0
card (Leaf v) = 1
card (Node v left right) = 1 + card(left) + card(right)

union :: Ord a => Set a -> Set a -> Set a
union set1 set2 = makeSet (toArray set1 ++ toArray set2)

-- TODO: Needs testing:
equals :: Ord a => Eq a => Set a -> Set a -> Bool 
equals set1 set2 = if card (set1) /= card (set2)
					then False
				   else if (card (union set1 set2) == card set1)
				   	then True
				   	else 
				   		False

intersect :: Ord a => Eq a => Set a -> Set a -> Set a
intersect Empty Empty = Empty
intersect set1 set2 = makeSet (listIntersection (toArray set1) (toArray set2))


subset :: Eq a => Set a -> Set a -> Bool 
subset set1 set2 = listSubset (toArray set1) (toArray set2)

select :: Ord a => (a -> Bool) -> Set a -> Set a
select cond set = makeSet (listSelect cond (toArray set)) 




-- delete the smallest element from the BST, and return modified tree lus the smallest element:
-- delSmallest :: Set a -> (Set a,a)
-- delSmallest (Leaf v) = (Empty,v)
-- delSmallest (Node v left right) = 
-- delSmallest (Node v Empty right) = 
-- delSmallest (Node v left Empty) = 





-- del :: Eq a => a -> Set a -> Set a 
-- del val Empty = Empty 
-- -- case 1: removing a node with no children (i.e a leaf:)
-- del val (Leaf v) = if v == val 
-- 					then Empty
-- 				   else 
-- 				   	(Leaf v)	
-- del val (Node v left right) = if val == v
-- 								then ...
-- 							  else if val < v 	
-- 							  	then del val left
-- 							  else --if val >= v 
-- 							  	then del val right 







\end{code}


