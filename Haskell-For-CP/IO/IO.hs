rep :: Int -> IO()
rep 0 = return () -- returns if not testcases are left to execute
rep t = do 
    n <- getLine 
    print n
    rep $ t - 1 -- recursive step

--IO type can be given to main as we will handle all our input output with the main function.
main :: IO()
-- our main will have multiple lines to get input and give ouput hence i will add a do.
main = do 
    --inputs can be taken with getLine the format will be string.
    n <- getLine
    --if n is a single integer
    let n_input = read n :: Int
    --read would let us read it like an integer
    a <- getLine 
    let arr_input = map read $ words a :: [Int]
    --This would let us take input as space seperated integers

    --i prefer to use print for output
    print a 
    print n

    --Sometimes we need to repeat it for t test cases i use recursion for that.
    t <- getLine 
    let tx = read t :: Int
    rep tx
