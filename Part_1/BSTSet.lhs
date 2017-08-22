\begin{code}
-- helper function:
addAll :: Ord a => [a] -> Set a
addAll [] = Empty
addAll (x:xs) = if x `elem` xs -- no duplicates allowed!
					then addAll(xs)
				else
					add x (addAll (xs))  
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









\end{code}


