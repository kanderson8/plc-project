module Graph where
import qualified Data.ByteString.Char8 as C

type Vertex = C.ByteString

type Edge = (C.ByteString,Vertex,Vertex)

data UndirectedGraph = UndirectedGraph [Vertex] [Edge]
  deriving (Show)
