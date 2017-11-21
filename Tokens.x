{
module Tokens where
import qualified Data.ByteString.Char8 as C
}

%wrapper "basic"

tokens :-

  $white+                       ;
  [A-Z]+    { \s -> TokenId (C.pack s) }
  [0-9]+     {\s -> TokenCost(C.pack s)}
  "vertices"     { \s -> TokenVertices}
  "edges"     { \s -> TokenEdges}  
{

-- The token type:
data Token  = TokenId C.ByteString |
             TokenEdges |
             TokenVertices |
             TokenCost C.ByteString 
             
             
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
