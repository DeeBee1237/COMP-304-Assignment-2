\begin{code}

-- Unordered set: 
type Set a = [a]

makeSet :: Eq a => [a] -> Set a
makeSet [] = []
makeSet (x:xs) = if x `elem` xs
					then makeSet(xs)
				else
					x : makeSet(xs)

has :: Eq a => a -> Set a -> Bool 
has a [] = False
has a (x:xs) = if x == a
				then True
			   else
			   	 has a (xs)

card :: Set a -> Int
card [] = 0
card (x:xs) = 1 + card (xs)


add :: Eq a => a -> Set a -> Set a
add a [] = [a]
add a set = if a `elem` set
				 then set
			    else [a] ++ set

del :: Eq a => a -> Set a -> Set a
del a [] = []
del a (x:xs) = if not (a `elem` (x:xs)) -- if the element to delete is already in the set, leave the set unchanged
				then (x:xs)
			else if a == x
			   	then del a xs
			else
			   	 x : del a xs

union :: Eq a => Set a -> Set a -> Set a
union [] [] = [] -- union of two empty sets, should be an empty set
union set1 set2 = makeSet(set1 ++ set2)


intersect :: Eq a => Set a -> Set a -> Set a
intersect [] [] = []
intersect set1 [] = []
intersect [] set2 = []
intersect set1 (x:xs) = if x `elem` set1 -- set 1 is what we check against, the other is what we recurse over
							then x : intersect set1 xs
						else 
							intersect set1 xs


equals :: Eq a => Set a -> Set a -> Bool 
equals [] [] = True
equals set1 [] = False
equals [] set2 = False -- sets can't be equal if they differ in length, given the uniqueness constraint:
equals (x:xs) (y:ys) = if length (x:xs) /= length (y:ys) 
						then False
					-- if the head of set1 is in set2 then delete the head of set1 from both sets, and recurse:
					else if (x `elem` (y:ys)) 
						then equals (del x (x:xs)) (del x (y:ys))
					-- if the head of set1 is not in set2, then the sets cannot be equal:
					else 
						False

subset :: Eq a => Set a -> Set a -> Bool 
subset [] set2 = True -- an empty set is always a subset of any non-empty set
subset set1 [] = False -- a non empty set can't be a subset of an empty set 
subset (x:xs) set2 = if (x `elem` set2)
						then subset xs set2
					else
						False

-- may need to revise this later
select :: (a -> Bool) -> Set a -> Set a 
select a set = filter a set

\end{code}