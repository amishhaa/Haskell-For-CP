import Control.Monad (replicateM)
import Data.List

solve :: String -> String -> Int -> Int
solve w s i
    | null w && null s = i
    | null w = i + length s
    | null s = i + length w
    | head w == head s = solve (drop 1 w) (drop 1 s) i
    | otherwise = solve (drop 1 w) (drop 1 s) (i + 1)

main :: IO ()
main = do
    n <- readLn
    results <- replicateM n $ do
        w <- getLine
        let s = "codeforces"
        return $ solve w s 0
    mapM_ print results