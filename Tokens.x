{
module Tokens where
import qualified Data.ByteString.Char8 as C
}

%wrapper "basic"

tokens :-

  $white+                       ;
  "VERTICES"     { \s -> TokenVertices}
   "EDGES"     { \s -> TokenEdges} 
  [a-zA-Z]+    { \s -> TokenId (C.pack s) }
  [0-9]+     {\s -> TokenCost(C.pack s)}
{

-- The token type:
data Token  = 
             TokenEdges |
             TokenVertices |
             TokenId C.ByteString |
             TokenCost C.ByteString 
             
             
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
