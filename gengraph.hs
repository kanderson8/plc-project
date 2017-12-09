import Data.List
import Data.Hashable
import Control.Monad
import Control.Monad.Trans.State.Lazy
import System.Environment
import System.Random
import Graph
import Grammar
import Tokens
import Prim
import Kruskal
import System.Environment
import Data.Maybe
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C
{- RandomState uses the state monad to represent a computation of a value of type a
   where the computation may obtain pseudo-random numbers from a RandomGen g -}
type RandomState g a = State g a

type Edge = (Int, Char, Char)

{- given a pair containing a lower and an upper bound for a range (see the docs for Random),
   compute a random value in that range, in the RandomState monad. -}
   --it has type (a, State)
randomRS :: (RandomGen g , Random a) => (a , a) -> RandomState g a
randomRS p = do
  rnd <- get
  let (v,rnd2) = randomR p rnd
  put rnd2
  return v

outputGraph :: [Char] -> [Edge] -> String
outputGraph c e = "VERTICES\n\n" ++ (intersperse ' ' c) ++ "\n\nEDGES\n\n" ++ edgesToString e 

edgesToString :: [Edge] -> String
edgesToString (e:es) = edgeToString e ++ "\n" ++ edgesToString es
edgesToString [] = []

edgeToString :: Edge -> String
edgeToString (cost, v1, v2) = (show cost) ++ " " ++ [v1] ++ " " ++ [v2]


{- This function takes a list of characters already in the graph representing the nodes that are already in
the graph and a character representing the node being added to the graph and it returns an edge that associates
a cost with the new node and a random node from the existing graph
-}
addEdge :: RandomGen g => [Char] -> Char -> RandomState g Edge
addEdge cs new = do
  sel <- randomRS(1, (length cs) :: Int)
  let vert = last (take sel cs)
  cost <- randomRS(1, 10 :: Int)
  return (cost, vert, new)
  
{- This function takes in a list of characters representing the vertices already in the graph and a list of characters
representing the vertices that still need to be added to the graph and it returns a list of edges associated with the graph -}
generateGraph :: RandomGen g => [Char] -> [Char] -> [Edge] -> RandomState g [Edge]
generateGraph old [] e = do
  buildMoreEdges e old 20

generateGraph old new e = do
  let next = head new
  edge <- addEdge old next
  generateGraph (old ++ [next]) (tail new) (e ++ [edge])
  
buildMoreEdges :: RandomGen g => [Edge] -> [Char] -> Int -> RandomState g [Edge]
buildMoreEdges e cs n = do
  sel <- randomRS(1, (length cs) :: Int)
  (num, v1, v2) <- addEdge cs (last (take sel cs))
  if n > 0 then if v1 /= v2 then buildMoreEdges (e ++ [(num, v1, v2)]) cs (n-1) else buildMoreEdges e cs (n-1) else return e

--writes to file
writeStringsToTxtFiles :: String -> Int -> [String] -> IO ()
writeStringsToTxtFiles name num (contents : rest) =
  writeFile (name ++ "-" ++ show num ++ ".txt") contents >>
  writeStringsToTxtFiles name (num + 1) rest
writeStringsToTxtFiles _ _ [] = return ()

writeStringsToGvFiles :: String -> Int -> [String] -> IO ()
writeStringsToGvFiles name num (contents : rest) =
  writeFile (name ++ "-" ++ show num ++ ".gv") contents >>
  writeStringsToGvFiles name (num + 1) rest
writeStringsToGvFiles _ _ [] = return ()

main :: IO ()
main = do 
  args <- getArgs
  let num = read (head args) :: Int
  let vertices = take num "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  let gen = mkStdGen num
  let (startV, rest) = splitAt 1 vertices
  let edges = evalState (generateGraph startV rest []) gen
  writeFile "graph.txt" (outputGraph vertices (edges))
  s <- readFile "graph.txt"
  writeStringsToTxtFiles "prim" 0 $ primTxt $ parseUndirectedGraph $ scanTokens s
  writeStringsToGvFiles "prim" 0 $ primGv $ parseUndirectedGraph $ scanTokens s
  writeStringsToTxtFiles "kruskal" 0 $ kruskalTxt $ parseUndirectedGraph $ scanTokens s
  writeStringsToGvFiles "kruskal" 0 $ kruskalGv $ parseUndirectedGraph $ scanTokens s
  return ()
