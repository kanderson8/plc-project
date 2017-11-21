{
module Tokens where
import qualified Data.ByteString.Char8 as C
}

%wrapper "basic"

tokens :-

  $white+                       ;
  [a-z]+    { \s -> TokenId (C.pack s) }
  [0-9]+    { \s -> TokenCost (C.pack s) }
  "VERTICES:"     { \s -> TokenVertices}
  "EGDES:"     { \s -> TokenEdges}  
  ";"          { \s -> TokenSemi}
{

-- The token type:
data Token  = TokenId C.ByteString |
             TokenCost C.ByteString |
             TokenVertices |
             TokenEdges |
             TokenSemi
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
