import Data.Char (digitToInt)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


unCompactFileSystem :: [Int] -> Vector Int
unCompactFileSystem diskMap =
    let go acc _ _ [] = V.concat (reverse acc)
        go acc isFile currentId (f : fs) =
            let k = if isFile then currentId else (-1)
                block = V.replicate f k
                nextId = if isFile then currentId + 1 else currentId
            in go (block : acc) (not isFile) nextId fs
    in go [] True 0 diskMap


rearrangeBlocks :: Vector Int -> [Int]
rearrangeBlocks fileSystem =
    let go acc ix rightIx
            | ix > rightIx = reverse acc
            | otherwise = case (fileSystem ! ix, fileSystem ! rightIx) of
                (-1, -1) -> go acc ix (rightIx - 1)
                (-1, y) -> go (y : acc) (ix + 1) (rightIx - 1)
                (x, _) -> go (x : acc) (ix + 1) rightIx
    in go [] 0 (V.length fileSystem - 1)


calcFileSystemCheckSum :: [Int] -> Int
calcFileSystemCheckSum diskMap =
    let fileSystem = unCompactFileSystem diskMap
        compactedFileSystem = rearrangeBlocks fileSystem
    in sum $ zipWith (*) compactedFileSystem [0..]


main :: IO ()
main = do
    diskMap <- map digitToInt <$> getLine
    print $ calcFileSystemCheckSum diskMap
