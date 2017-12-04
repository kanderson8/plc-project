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
traverseKruskal (UndirectedGraph a b) n d f = (UndirectedGraph d f) : if ((getNextHelpfulEdge b d f n) == 0) then [] else  traverseKruskal (UndirectedGraph a b) ((getNextHelpfulEdge b d f n) + 1) (d ++ (addVs (getEdgeAtN b (getNextHelpfulEdge b d f n)) d))  (f ++ [(getEdgeAtN b (getNextHelpfulEdge b [] f n))])

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
edgeHelpful e done f = True


                                                        
