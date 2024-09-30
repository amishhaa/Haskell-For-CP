import Control.Monad (replicateM)
import Data.List

solve :: [Int] -> Int -> Int -> Int -> Int
solve a f c d
    |null a = max c d
    |(f == 0) && (head a == 0) = solve (tail a) f (c + 1) (max d c)
    |(f == 0) && (head a == 1) = solve (tail a) 1 0 (max d c)
    |(f == 1) && (head a == 0) = solve (tail a) 0 (c + 1) (max d c)
    |otherwise = solve (tail a) 1 c d

main :: IO ()
main = do
    n <- readLn
    results <- replicateM n $ do
        n <- getLine
        w <- fmap words getLine
        let b = map read w :: [Int]
        return $ solve b 1 0 0
    mapM_ print results