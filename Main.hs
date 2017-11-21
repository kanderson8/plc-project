import Graph
import Grammar
import Tokens
import System.Environment
import Data.Maybe
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C


process :: UndirectedGraph -> [String]
process a = [show a]


writeStringsToFiles :: String -> Int -> [String] -> IO ()
writeStringsToFiles name num (contents : rest) =
  writeFile (name ++ "-" ++ show num ++ ".txt") contents >>
  writeStringsToFiles name (num + 1) rest
writeStringsToFiles _ _ [] = return ()

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  writeStringsToFiles "test" 0 $ process $ parseUndirectedGraph $ scanTokens s
  
