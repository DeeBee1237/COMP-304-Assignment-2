\begin{code}

type Set a = [a]
type Graph a b = (Set a, Set (a,b,a))
type Path a b = [(a,b,a)]

-- Helper Code:

-- is it true that there is some (first) value from the tuples, not in the list of vertices?
firstFromTupleNotInVert :: Eq a => [a] -> [(a,b,a)] -> Bool
firstFromTupleNotInVert vertices [] = False
firstFromTupleNotInVert vertices ((x,y,z):rest) = if not (x `elem` vertices)
													  then True
												  else
												  	firstFromTupleNotInVert vertices rest

-- is it true that there is some (last) value from the tuples, not in the list of vertices?
lastFromTupleNotInVert :: Eq a => [a] -> [(a,b,a)] -> Bool
lastFromTupleNotInVert vertices [] = False
lastFromTupleNotInVert vertices ((x,y,z):rest) = if not (z `elem` vertices)
													  then True
												  else
												  	lastFromTupleNotInVert vertices rest
-- check for repeated vertices:
repeatedVert :: Eq a => [a] -> Bool
repeatedVert [] = False
repeatedVert (x:xs) = if x `elem` xs
						then True
					  else
					  	repeatedVert xs

-- check for repeated Edges:
repeatedEdges :: Eq a => Eq b =>  [(a,b,a)] -> Bool
repeatedEdges [] = False
repeatedEdges ((x,y,z):rest) = if (x,y,z) `elem` rest
						then True
					  else
					  	repeatedEdges rest

\end{code}


\begin{code}

-- STRETCH GOAL: maybe provide specific instanecs for error messages later
makeGraph :: Eq a => Eq b => ([a], [(a,b,a)]) -> Graph a b
makeGraph ([],[]) =  ([],[])
makeGraph (vert, edges) = if (firstFromTupleNotInVert vert edges) || (lastFromTupleNotInVert vert edges)
							then error "There is an edge with an unknown vertex"
						else if repeatedVert vert
							then error "There are repeated vertices, vertices should be unique"
						else if repeatedEdges edges
							then error "There are repeated edges, edges should be unique"
						  else
						  	(vert, edges)


predecessors :: Eq a => Graph a b -> a -> Set a
predecessors (vert,[]) vertex = []
predecessors (vert, ((x,y,z):rest)) vertex = if z == vertex
												then x :  predecessors (vert,rest) vertex
											else 
												predecessors (vert,rest) vertex


successors :: Eq a => Graph a b -> a -> Set a
successors (vert,[]) vertex = []
successors (vert, ((x,y,z):rest)) vertex = if x == vertex
											then z : successors (vert,rest) vertex
										   else 
											successors (vert,rest) vertex


-- TODO:
isConnected :: Graph a b -> a -> Bool
isConnected graph vertex = False 


\end{code}

\begin{verbatim}

some tests from the command line:

*Main> let x = makeGraph (["c","b","d"],[("c",9,"b"),("b",1,"c"),("b",2,"d"),("d",1,"b"),("c",3,"d"),("d",8,"c")])
*Main> x
(["c","b","d"],[("c",9,"b"),("b",1,"c"),("b",2,"d"),("d",1,"b"),("c",3,"d"),("d",8,"c")])
*Main> predecessors x "d"
["b","c"]
*Main> 
*Main> 
*Main> predecessors x "c"
["b","d"]
*Main> predecessors x "b"
["c","d"]

let y = makeGraph (["a","b","c","d"],[("c",1,"a"),("c",3,"b"),("c",4,"d"),("b",2,"a"),("d",5,"b")])

\end{verbatim}






