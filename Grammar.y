{
module Grammar where
import Tokens
import Graph
}

%name parseUndirectedGraph
%tokentype { Token }
%error { parseError }

%token
    id { TokenId $$ }
    cost {TokenCost $$}
    VERTICES { TokenVertices }
    EDGES { TokenEdges }
%%

--productions
Graph : VERTICES Vs EDGES Es {UndirectedGraph $2 $4}

-- ROOTS: [a, ... , z]
Vs:
    ids {$1}

-- [a] or [a, ..., z]
Es:
   E {[$1]}
|  E Es {$1:$2}

-- a -> as;
E:
cost id id{($1,$2,$3)}

-- [a] or [a, ..., z]
ids:
   id {[$1]}
|  id ids {$1:$2}

{

parseError :: [Token] -> a
parseError ts = error ("Parse error (here): " ++ show ts)

}
