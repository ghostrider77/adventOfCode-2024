import Data.Char (digitToInt)
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as S

data Block = Block { startIx :: Int, size :: Int, fileId :: Int } deriving Show

instance Eq Block where
    (==) :: Block -> Block -> Bool
    Block {startIx = ix1} == Block {startIx = ix2} = ix1 == ix2

instance Ord Block where
    (<=) :: Block -> Block -> Bool
    Block {startIx = ix1} <= Block {startIx = ix2} = ix1 <= ix2


parseFileSystem :: [Int] -> (Set Block, [Block])
parseFileSystem diskMap =
    let go acc _ _ _ [] =
            let allBlocks = reverse $ filter (\Block {size} -> size /= 0) acc
                (spaces, files) = partition (\(Block {fileId}) -> fileId == -1) allBlocks
            in (S.fromAscList spaces, reverse files)
        go acc isFile startIx currentId (f : fs) =
            let k = if isFile then currentId else -1
                block = Block { startIx = startIx, size = f, fileId = k }
                nextId = if isFile then currentId + 1 else currentId
            in go (block : acc) (not isFile) (startIx + f) nextId fs
    in go [] True 0 0 diskMap


rearrangeBlocks :: Set Block -> [Block] -> [Block]
rearrangeBlocks emptyBlocks fileBlocks =
    let go acc _ [] = acc
        go acc freeSpaces (fb@Block {startIx = fileStartIx, size = fileSize} : fbs) =
            let validSpaces = S.filter (\(Block {startIx = spaceStartIx}) -> spaceStartIx < fileStartIx) freeSpaces
            in case S.lookupMin $ S.filter (\(Block {size = spaceSize}) -> fileSize <= spaceSize) validSpaces of
                Nothing -> go (fb : acc) validSpaces fbs
                Just space@Block {startIx, size = spaceSize} ->
                    let remainingSpace = space {startIx = startIx + fileSize, size = spaceSize - fileSize}
                        validSpaces' =
                            if spaceSize == fileSize then S.delete space validSpaces
                            else S.insert remainingSpace (S.delete space validSpaces)
                    in go ((fb {startIx = startIx}) : acc) validSpaces' fbs
    in go [] emptyBlocks fileBlocks


calcFileSystemCheckSum :: [Int] -> Int
calcFileSystemCheckSum diskMap =
    let (emptyBlocks, fileBlocks) = parseFileSystem diskMap
        compactedFileSystem = rearrangeBlocks emptyBlocks fileBlocks
        blockCheckSum (Block {startIx, size, fileId}) = sum $ map (*fileId) [startIx..(startIx+size-1)]
    in sum $ map blockCheckSum compactedFileSystem


main :: IO ()
main = do
    diskMap <- map digitToInt <$> getLine
    print $ calcFileSystemCheckSum diskMap
