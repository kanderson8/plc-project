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

traversePrim :: UndirectedGraph -> [V] ->[E] -> [UndirectedGraph]
traversePrim (UndirectedGraph a b) [] e = (UndirectedGraph [] []) :
  (traversePrim (UndirectedGraph a b) ([(firstV (findMinEdge b (minEdge b (getCost (head b)) ) )),(secondV (findMinEdge b (minEdge b (getCost (head b)) ) ))]) ([findMinEdge b (minEdge b (getCost (head b)) ) ]))
traversePrim (UndirectedGraph a b) v e = [(UndirectedGraph v e)]

firstV :: E -> V
firstV (_,v,_) = v

secondV :: E -> V
secondV (_,_,v) = v

minEdge :: [E] -> Int -> Int
minEdge (e:[]) min = if (min > getCost e) then getCost e else min
minEdge (e:es) min  = if (min > getCost e) then minEdge es (getCost e)
                                    else minEdge es (min)

findMinEdge :: [E] -> Int -> E
findMinEdge (e:es) min = if (getCost e == min) then e else findMinEdge es min 

getCost :: E -> Int
getCost (cost, _, _) = cost

  
