module Prim where
import Graph
import Data.Maybe
import Data.List
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C

type Done = [V] -- Vertices added to MST so far
type FinalEdges = [E] -- Final edges that are included in final MST

--takes an UndirectedGraph and outputs a String of each state in V[] E[] .txt format
primTxt :: UndirectedGraph -> [String]
primTxt (UndirectedGraph a b) = show (UndirectedGraph a b) : toString (traversePrim (UndirectedGraph a b) [] [])

--takes and UnirectedGraph and outpus a String of each state in graphviz format
primGv :: UndirectedGraph -> [String]
primGv (UndirectedGraph a b) = vizHelper (UndirectedGraph a b) : (toGraphViz (traversePrim (UndirectedGraph a b) [] []))

--[UndirectedGraph] to basic show of each state
toString :: [UndirectedGraph] -> [String]
toString (a:as) = show a : toString as
toString [] = []

--[UndirectedGraph] to graphviz of each state using helpers
toGraphViz :: [UndirectedGraph] -> [String]
toGraphViz (g:gs) = vizHelper g : toGraphViz gs
toGraphViz [] = []

-- Top of the file and calls helpers to fill in vertices and costs between
vizHelper :: UndirectedGraph -> String
vizHelper (UndirectedGraph a b) = "graph Prim {\n" ++ traverseEdges b ++"}"

--Take the [E] and converts each edge to a string representation using helpers
traverseEdges :: [E] -> String
traverseEdges (e:es) =vizEdge e ++ (traverseEdges es)
traverseEdges [] = []

-- Takes and edge and represents it as a String in graphviz format
vizEdge :: E -> String
vizEdge (cost, v1, v2) = (C.unpack v1) ++ " [ shape = circle];\n" ++ (C.unpack v2) ++ " [shape = circle];\n" ++ (C.unpack v1) ++ " -- " ++ (C.unpack v2) ++ " [label = " ++ (show cost) ++" ];\n"

{-takes the state of the UndirectedGraph, the included edges so far in the MST,
 the included vertices so far in MST, and strings the states together into an
 [UnidirectedGraph] utilizing helpers that I made the name of too long -}
traversePrim :: UndirectedGraph -> Done->FinalEdges -> [UndirectedGraph]
traversePrim (UndirectedGraph a b) [] e = (UndirectedGraph [] []) :
  (traversePrim (UndirectedGraph a b) ([(firstV (findMinEdge b (minEdge b (getCost (head b)) ) )),(secondV (findMinEdge b (minEdge b (getCost (head b)) ) ))]) ([findMinEdge b (minEdge b (getCost (head b)) ) ]))
traversePrim (UndirectedGraph a b) v e = if (doneTraversal (UndirectedGraph a b) v) then [(UndirectedGraph v e)] else (UndirectedGraph v e) : (traversePrim (UndirectedGraph a b) (combineInDone v (firstV (findMinEdge (findEdges (UndirectedGraph a b) v) (minEdge (findEdges (UndirectedGraph a b) v) (getCost (head (findEdges (UndirectedGraph a b) v))) ) )) (secondV (findMinEdge (findEdges (UndirectedGraph a b) v) (minEdge (findEdges (UndirectedGraph a b) v) (getCost (head (findEdges (UndirectedGraph a b) v))) ) ))))  (e ++[findMinEdge (findEdges (UndirectedGraph a b) v) (minEdge (findEdges (UndirectedGraph a b) v) (getCost (head (findEdges (UndirectedGraph a b) v))) ) ])

--adds vertices to set of vertices in final MST set of [V] depending on which already there
combineInDone :: Done -> V -> V -> Done
combineInDone done v1 v2 = if (vertexInDone v1 done && vertexInDone v2 done) then done  else if (vertexInDone v1 done) then done ++ [v2] else if (vertexInDone v2 done) then done ++ [v1] else done ++ [v1] ++ [v2]

--checks if at the MST
doneTraversal :: UndirectedGraph -> Done -> Bool
doneTraversal (UndirectedGraph a b) v =if (verticesInDone a v) then True else False

--checks if the every V in [V] is in the final MST set of [V]
verticesInDone :: [V] -> Done -> Bool
verticesInDone (v:[]) done = if (vertexInDone v done) then True else False
verticesInDone (v:vs) done = if (vertexInDone v done && verticesInDone vs done) then True else False
verticesInDone [] _ = False

--checks if a vertex is in the final MST set of [V]
vertexInDone :: V -> Done -> Bool
vertexInDone v (vd:[]) = if (v == vd) then True else False
vertexInDone v (vd:vds) = if (v == vd || vertexInDone v vds) then True else False
vertexInDone _ [] = False

--grabs the edges in the UndirectedGraph that are connected to the current MST at this stage according to Prim's Algorithm by looking at the final MST set of [V]
findEdges :: UndirectedGraph -> Done  -> [E]
findEdges (UndirectedGraph a b) done  = newEdges b done 

--checks if found a valid edge to put into the possibly edge additions at this step
newEdges :: [E] ->Done -> [E]
newEdges (e:es) done = if (vsFromEinDone e done) then  (newEdges es done) else e: (newEdges es done)
newEdges [] _  = []

--after the initial edge is added, new edges to consider must have exactly one of its two vertices be in the current MST. Both vertices already in, not needed. No vertices in, not considered according to Prim's algorithm.
vsFromEinDone :: E -> Done -> Bool
vsFromEinDone (_,v1,v2) done = if (vertexInDone v1 done && vertexInDone v2 done) then True else if (vertexInDone v1 done) then False else if (vertexInDone v2 done) then False else True

--checks if the Edge is already in the final edges of the MST
edgeInDone :: E ->FinalEdges -> Bool
edgeInDone e (fe:fes) = if ((edgesAreEqual e fe) || (edgeInDone e fes)) then True else False
edgeInDone e [] = False

--checks if edges are equal by comparing all components (wasn't sure if just e1==e2 would work for sure)
edgesAreEqual :: E -> E -> Bool
edgesAreEqual (ca , v1a, v2a) (cb, v1b ,v2b) = if (ca == cb && v1a == v1b && v2a == v2b) then True else False

--Gets first vertex of an edge
firstV :: E -> V
firstV (_,v,_) = v

--Gets second vertex of an edge
secondV :: E -> V
secondV (_,_,v) = v

--given set of edges, returns the minimum cost
minEdge :: [E] -> Int -> Int
minEdge (e:[]) min = if (min > getCost e) then getCost e else min
minEdge (e:es) min  = if (min > getCost e) then minEdge es (getCost e)
                                    else minEdge es (min)

--given the minimum cost, finds the first edge with that cost from [E]
findMinEdge :: [E] -> Int -> E
findMinEdge [] _ = (100 , C.empty, C.empty)
findMinEdge (e:[]) _ = e
findMinEdge (e:es) min = if (getCost e == min) then e else findMinEdge es min

--gets the cost of an Edge
getCost :: E -> Int
getCost (cost, _, _) = cost

  
