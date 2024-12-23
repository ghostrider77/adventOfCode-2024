import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Edge = Edge String String deriving Show
newtype Clique = Clique { clique :: [String] } deriving (Eq, Ord)
newtype Graph = Graph { adjacencyList :: Map String (Set String) } deriving Show


instance Show Clique where
    show :: Clique -> String
    show Clique {clique} = intercalate "," clique


parseInput :: [String] -> [Edge]
parseInput content =
    let readEdge line = case splitOn "-" line of
            [a, b] -> Edge a b
            _ -> error "Malformed input."
    in map readEdge content


createGraph :: [Edge] -> Graph
createGraph edges =
    let insertUndirectedEdge m (Edge a b) =
            M.insertWith S.union b (S.singleton a) $ M.insertWith S.union a (S.singleton b) m
    in Graph {adjacencyList = foldl insertUndirectedEdge M.empty edges}


getNeighbors :: Graph -> String -> Set String
getNeighbors Graph {adjacencyList} node = M.findWithDefault S.empty node adjacencyList


getConnectedTriples :: Graph -> Set Clique
getConnectedTriples graph@Graph {adjacencyList} =
    let areNeighbors a b = let ns = getNeighbors graph a in S.member b ns
        getTriples node =
            let ns = S.toList $ getNeighbors graph node
                triples = [sort [node, n1, n2] | n1 <- ns, n2 <- ns, n1 /= n2, areNeighbors n1 n2]
            in foldl (\acc t -> S.insert (Clique {clique = t}) acc) S.empty triples
    in foldl (\acc node -> S.union acc (getTriples node)) S.empty (M.keys adjacencyList)


extendCliques :: Graph -> Set Clique -> Set Clique
extendCliques graph cliques =
    let extend (Clique {clique}) = case clique of
            [] -> S.empty
            node : nodes ->
                let commonNeighbors =
                        let neighbors = getNeighbors graph node
                        in foldl (\acc n -> S.intersection acc (getNeighbors graph n)) neighbors nodes
                in S.foldl (\acc n -> S.insert (Clique {clique = sort (n : clique)}) acc) S.empty commonNeighbors
    in S.foldl (\acc c -> S.union acc (extend c)) S.empty cliques


findLargestClique :: Graph -> String
findLargestClique graph =
    let triples = getConnectedTriples graph
        go cliques
            | S.size cliques == 1 = show $ S.findMin cliques
            | otherwise = go (extendCliques graph cliques)
    in go triples


main :: IO ()
main = do
    content <- lines <$> getContents
    let edges = parseInput content
    let graph = createGraph edges
    putStrLn $ findLargestClique graph
