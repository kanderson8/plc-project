module Prim where
import Graph
import Data.Maybe
import Data.List
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C

type Done = [V]
type FinalEdges = [E]

prim :: UndirectedGraph -> [String]
prim (UndirectedGraph a b) = toString (traversePrim (UndirectedGraph a b) [] [])

toString :: [UndirectedGraph] -> [String]
toString (a:as) = show a : toString as
toString [] = []

traversePrim :: UndirectedGraph -> Done->FinalEdges -> [UndirectedGraph]
traversePrim (UndirectedGraph a b) [] e = (UndirectedGraph [] []) :
  (traversePrim (UndirectedGraph a b) ([(firstV (findMinEdge b (minEdge b (getCost (head b)) ) )),(secondV (findMinEdge b (minEdge b (getCost (head b)) ) ))]) ([findMinEdge b (minEdge b (getCost (head b)) ) ]))
traversePrim (UndirectedGraph a b) v e = if (doneTraversal (UndirectedGraph a b) v) then [(UndirectedGraph v e)] else (UndirectedGraph v e) : (traversePrim (UndirectedGraph a b) (v ++ [(firstV (findMinEdge (findEdges (UndirectedGraph a b) v e) (minEdge (findEdges (UndirectedGraph a b) v e) (getCost (head (findEdges (UndirectedGraph a b) v e))) ) )),(secondV (findMinEdge (findEdges (UndirectedGraph a b) v e) (minEdge (findEdges (UndirectedGraph a b) v e) (getCost (head (findEdges (UndirectedGraph a b) v e))) ) ))])  (e ++[findMinEdge (findEdges (UndirectedGraph a b) v e) (minEdge (findEdges (UndirectedGraph a b) v e) (getCost (head (findEdges (UndirectedGraph a b) v e))) ) ]))


doneTraversal :: UndirectedGraph -> Done -> Bool
doneTraversal (UndirectedGraph a b) v =if (verticesInDone a v) then True else False

verticesInDone :: [V] -> Done -> Bool
verticesInDone (v:[]) done = if (vertexInDone v done) then True else False
verticesInDone (v:vs) done = if (vertexInDone v done && verticesInDone vs done) then True else False
verticesInDone [] _ = False

vertexInDone :: V -> Done -> Bool
vertexInDone v (vd:[]) = if (v == vd) then True else False
vertexInDone v (vd:vds) = if (v == vd || vertexInDone v vds) then True else False
vertexInDone _ [] = False

findEdges :: UndirectedGraph -> Done -> FinalEdges -> [E]
findEdges (UndirectedGraph a b) v e = newEdges b e

newEdges :: [E] ->FinalEdges -> [E]
newEdges (e:es) fEs = if (edgeInDone e fEs) then  (newEdges es fEs) else e: (newEdges es fEs)
newEdges [] _  = []

edgeInDone :: E ->FinalEdges -> Bool
edgeInDone e (fe:fes) = if ((edgesAreEqual e fe) || (edgeInDone e fes)) then True else False
edgeInDone e [] = False

edgesAreEqual :: E -> E -> Bool
edgesAreEqual (ca , v1a, v2a) (cb, v1b ,v2b) = if (ca == cb && v1a == v1b && v2a == v2b) then True else False

firstV :: E -> V
firstV (_,v,_) = v

secondV :: E -> V
secondV (_,_,v) = v

minEdge :: [E] -> Int -> Int
minEdge (e:[]) min = if (min > getCost e) then getCost e else min
minEdge (e:es) min  = if (min > getCost e) then minEdge es (getCost e)
                                    else minEdge es (min)

findMinEdge :: [E] -> Int -> E
findMinEdge [] _ = (100 , C.empty, C.empty)
findMinEdge (e:[]) _ = e
findMinEdge (e:es) min = if (getCost e == min) then e else findMinEdge es min


getCost :: E -> Int
getCost (cost, _, _) = cost

  
