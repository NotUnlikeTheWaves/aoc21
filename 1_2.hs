main = do
    contents <- getContents
    -- [10, 18, 17, 20, 15, 23, 24]
    -- [18, 17, 20, 15, 23, 24]
    -- [x   y   n   y   n   y   y]

    let contentList = words contents
    let integerList = map strToInt contentList

    -- Addition to the previous one:
    let sum = getTriplesSummed integerList
    print sum

    let controlRow = init sum
    let diffRow = tail sum
    let compute = diff controlRow diffRow

    let foundPositive = filter (>0) compute
    
    print (length foundPositive)
    return ()

-- Take three elements and sum them as a 'moving window of 3' that can be compared the same way later
getTriplesSummed :: Num a => [a] -> [a] 
getTriplesSummed [] = [0] 
getTriplesSummed x = sum(take 3 x) : getTriplesSummed (tail x)

diff :: Num a => [a] -> [a] -> [a]
diff [] _ = [0]
diff _ [] = [0]
diff (x:xs) (y:ys) = (y-x) : diff xs ys

strToInt str = read str :: Int