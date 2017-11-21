module Graph where
import qualified Data.ByteString.Char8 as C
import Data.Maybe

type V = C.ByteString

type E = (Int,V,V)

type Ein = (C.ByteString,V,V)

data UndirectedGraph = UndirectedGraph [V] [E]
  deriving (Show)


eToEin :: [Ein] -> [E]
eToEin (e:es) = (convertToInt e) : (eToEin es)
eToEin [] = []

convertToInt :: Ein -> E
convertToInt (char, a, b) = ((charToInt char) , a , b)

charToInt :: C.ByteString -> Int
charToInt c = fromMaybeIntChar (C.readInt c)

fromMaybeIntChar :: Maybe (Int, C.ByteString) ->Int
fromMaybeIntChar Nothing = 0
fromMaybeIntChar (Just a) = fromTuple (fromJust (Just a))

fromTuple :: (Int,C.ByteString) -> Int
fromTuple (a,b) = a



