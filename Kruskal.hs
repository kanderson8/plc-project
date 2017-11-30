module Kruskal where
import Graph
import Prim
import Data.Maybe
import Data.List
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C


kruskal :: UndirectedGraph -> [String]
--kruskal a = [show a] {- initial graph -}
kruskal (UndirectedGraph a b) = show (UndirectedGraph a b) : toString (traverseKruskal (UndirectedGraph a (sortE b)) 0 [] [])

--This sorts the edges in Ascending order
sortE :: [E] -> [E]
sortE e = sort e

--not done
traverseKruskal :: UndirectedGraph -> Int -> Done -> FinalEdges -> [UndirectedGraph]
traverseKruskal (UndirectedGraph a b) n [] e =  (UndirectedGraph [] []) : if ((getNextHelpfulEdge b [] n) == 0) then [] else traverseKruskal (UndirectedGraph a b) ((getNextHelpfulEdge b [] n) + 1) [] []

getNextHelpfulEdge :: [E] -> Done ->Int -> Int
getNextHelpfulEdge e done n = if (length e)<n then 0 else if edgeHelpful (getEdgeAtN e n) done then n else getNextHelpfulEdge e done (n+1)


getEdgeAtN ::  [E] -> Int -> E
getEdgeAtN (e:es) 0 = e
getEdgeAtN (e:es) n = getEdgeAtN es (n-1)

--not done
edgeHelpful :: E -> Done -> Bool
edgeHelpful e done = True


                                                         
