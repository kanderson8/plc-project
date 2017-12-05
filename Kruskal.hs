module Kruskal where
import Graph
import Prim
import Data.Maybe
import Data.List
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C


kruskalTxt :: UndirectedGraph -> [String]
--kruskal a = [show a] {- initial graph -}
kruskalTxt (UndirectedGraph a b) = show (UndirectedGraph a b) : toString (traverseKruskal (UndirectedGraph a (sortE b)) 1 [] [])

kruskalGv :: UndirectedGraph -> [String]
kruskalGv (UndirectedGraph a b) = vizHelper (UndirectedGraph a b) : toGraphViz (traverseKruskal (UndirectedGraph a (sortE b)) 1 [] [])

--This sorts the edges in Ascending order
sortE :: [E] -> [E]
sortE e = sort e

--Traverses through each edge, adding only if helpful
traverseKruskal :: UndirectedGraph -> Int -> Done -> FinalEdges -> [UndirectedGraph]

traverseKruskal (UndirectedGraph a b) n [] f =  (UndirectedGraph [] []) :  traverseKruskal (UndirectedGraph a b) ((getNextHelpfulEdge b [] f n) + 1) (addVs (getEdgeAtN b (getNextHelpfulEdge b [] f n)) []) (f ++ [(getEdgeAtN b (getNextHelpfulEdge b [] f n))])

traverseKruskal (UndirectedGraph a b) n d f = (UndirectedGraph d f) : if ((getNextHelpfulEdge b d f n) == 0) then [] else  traverseKruskal (UndirectedGraph a b) ((getNextHelpfulEdge b d f n) + 1) (d ++ (addVs (getEdgeAtN b (getNextHelpfulEdge b d f n)) d))  (f ++ [(getEdgeAtN b (getNextHelpfulEdge b d f n))])

--given location in [E] give the next helpful edge or 0 if done
getNextHelpfulEdge :: [E] -> Done -> FinalEdges ->Int -> Int
getNextHelpfulEdge e done f n = if (length e)<n then 0 else if edgeHelpful (getEdgeAtN e n)  done f then n else getNextHelpfulEdge e done f (n+1)

--take and E and add Vs if not already in Done
addVs :: E -> Done -> [V]
addVs (_, v1, v2) done = if(vertexInDone v1 done && vertexInDone v2 done) then [] else if(vertexInDone v1 done) then [v2] else if (vertexInDone v2 done) then [v1] else [v1,v2]

--get an E for array of edges ar location n
getEdgeAtN ::  [E] -> Int -> E
getEdgeAtN (e:es) 1 = e
getEdgeAtN (e:[]) n = e
getEdgeAtN (e:es) n = getEdgeAtN es (n-1)


--not done
edgeHelpful :: E -> Done -> FinalEdges -> Bool
edgeHelpful (c, v1, v2) done f = 
  if not ((elem v1 done) && (elem v2 done))
  then True 
  else checkForCycle (c, v1, v2) [v1] (getConnected v1 [] f) f
  
--this takes in an edge to check, a list of vertices that have been visited, a list of edges to add, and a boolean
checkForCycle :: E -> [V] -> [E] -> FinalEdges -> Bool
checkForCycle (c, v1, v2) checked (e:es) f = 
  if (isConnected v2 e) 
  then False 
  else checkForCycle (c, v1, v2) (checked ++ [(getSecond e)]) (es ++ (getConnected (getSecond e) checked f)) f
checkForCycle _ _ [] _ = True 

getConnected :: V -> [V] -> FinalEdges -> [E]
getConnected v1 vs [] = []
getConnected v1 vs (e:es) = 
  if (checkEdge v1 e) && (not (beenVisited e vs)) 
  then [e] ++ (getConnected v1 vs es) --make sure these are the right args to pass to getConnected
  else (getConnected v1 vs es)

checkEdge :: V -> E -> Bool
checkEdge c (c1, v1, v2) = if c == v1 then True else False

isConnected :: V -> E -> Bool
isConnected v (c, v1, v2) = if v == v2 then True else False

--if the second vertex is already in the visited list then true else false
beenVisited :: E -> [V] -> Bool
beenVisited (c, v1, v2) vs = if elem v2 vs then True else False

getFirst :: E -> V
getFirst (c, v1, v2) = v1

getSecond :: E -> V
getSecond (c, v1, v2) = v2




                                                        
