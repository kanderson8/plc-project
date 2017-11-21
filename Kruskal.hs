module Kruskal where
import Graph
import Data.Maybe
import Data.List
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C


kruskal :: UndirectedGraph -> [String]
kruskal a = [show a]
