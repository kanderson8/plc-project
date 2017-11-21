module Graph where
import qualified Data.ByteString.Char8 as C

type V = C.ByteString

type E = (C.ByteString,V,V)

data UndirectedGraph = UndirectedGraph [V] [E]
  deriving (Show)

