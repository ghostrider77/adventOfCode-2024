import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Edge = Edge String String deriving Show
newtype Graph = Graph { adjacencyList :: Map String [String] } deriving Show


parseInput :: [String] -> [Edge]
parseInput content =
    let readEdge line = case splitOn "-" line of
            [a, b] -> Edge a b
            _ -> error "Malformed input."
    in map readEdge content


createGraph :: [Edge] -> Graph
createGraph edges =
    let insertUndirectedEdge m (Edge a b) = M.insertWith (++) b [a] $ M.insertWith (++) a [b] m
    in Graph {adjacencyList = foldl insertUndirectedEdge M.empty edges}


getNeighbors :: Graph -> String -> [String]
getNeighbors Graph {adjacencyList} node = M.findWithDefault [] node adjacencyList


getConnectedTriples :: Graph -> Set [String]
getConnectedTriples graph@Graph {adjacencyList} =
    let areNeighbors a b = b `elem` getNeighbors graph a
        getTriples node =
            let ns = getNeighbors graph node
            in S.fromList [sort [node, n1, n2] | n1 <- ns, n2 <- ns, n1 /= n2, areNeighbors n1 n2]
    in foldl (\acc node -> S.union acc (getTriples node)) S.empty (M.keys adjacencyList)


findTriplesWithT :: Graph -> Int
findTriplesWithT graph =
    let triples = getConnectedTriples graph
    in S.size $ S.filter (any (\node -> head node == 't')) triples


main :: IO ()
main = do
    content <- lines <$> getContents
    let edges = parseInput content
    let graph = createGraph edges
    print $ findTriplesWithT graph
