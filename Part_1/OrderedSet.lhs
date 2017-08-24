\begin{code}

-- Ordered set: 
type Set a = [a]

makeSet :: Ord a => Eq a => [a] -> Set a
makeSet [] = []
makeSet (x:xs) = add x (makeSet (xs)) -- just tried this, and it works!

add :: Ord a => Eq a => a -> Set a -> Set a
add a [] = [a] -- if the current head is smaller, then a needs to go somewhere further up the array
add a (x:xs) = if x < a 
				then x : add a xs
			    -- no duplicates
			   else if x == a
			   	then x : xs
			   -- now that x > a, a can't go any further, so we insert it bewteen the appropriate values
			   else 
			   	a : x : xs

-- implemented this using binary search, which performs much faster:
has :: Ord a => Eq a => a -> Set a -> Bool 
has a [] = False
			-- split the set in half and see if a is equal to less than or greater than this middle element:
has a set = if a == set !! ((length set) `div` 2) 
				then True
			else if a < set !! ((length set) `div` 2) 
				then has a (take ((length set) `div` 2) set)
			else --if a > set !! ((length set) `div` 2)
				has a (drop ((length set) `div` 2 + 1) set)

card :: Set a -> Int
card [] = 0
card (x:xs) = foldl (\inc x -> inc + 1) 1 xs


del :: Eq a => a -> Set a -> Set a
del a [] = []
del a (x:xs) = if not (a `elem` (x:xs)) -- if the element to delete is not in the set, leave the set unchanged
				then (x:xs)
			else if a == x
			   	then del a xs
			else
			   	 x : del a xs

union :: Eq a => Ord a => Set a -> Set a -> Set a
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
equals (x:xs) (y:ys) =  if (length (x:xs) /= length (y:ys))
							then False
						else if ((\a b -> a == b) x y)
							then equals xs ys
						else 
							False 

-- equals [] [] = True
-- equals set1 [] = False
-- equals [] set2 = False
-- equals set1 set2 = if length set1 /= length set2
-- 						then False
-- 					else if head set1 /= head set2
-- 						then False
-- 					else equals (tail set1) (tail set2)

					
subset :: Eq a => Set a -> Set a -> Bool 
subset [] set2 = True -- an empty set is always a subset of any non-empty set
subset set1 [] = False -- a non empty set can't be a subset of an empty set 
subset (x:xs) set2 = if (x `elem` set2)
						then subset xs set2
					else
						False


select :: (a -> Bool) -> Set a -> Set a 
select a set = filter a set

\end{code}