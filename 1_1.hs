-- lookup: 
-- function composition (can i run function composition directly with arguments)

import Data.Char

main = do
    contents <- getContents
    -- [10, 18, 17, 20, 15, 23, 24]
    -- [18, 17, 20, 15, 23, 24]
    -- [x   y   n   y   n   y   y]
    let li = words contents
    let input = map strToInt li
    let controlRow = init input
    let diffRow = tail input
    let compute = diff controlRow diffRow
    print compute

    let foundPositive = filter (>0) compute
    print foundPositive
    print (length foundPositive)
    return ()

diff :: Num a => [a] -> [a] -> [a]
diff [] _ = [0]
diff _ [] = [0]
diff (x:xs) (y:ys) = (y-x) : diff xs ys

strToInt str = read str :: Int