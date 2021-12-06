

main = do
    contents <- getContents

    let contentList = lines contents
    
    --mapM_ print contentList -- print content

    let splitted = map splitIntoDirectionAndValue contentList
        values = map getValue splitted
        collected = foldr (zipWith (+)) [0,0] values
        multiplied = product collected

    print collected
    print multiplied

    return ()


splitIntoDirectionAndValue :: String -> ([Char], Int)
splitIntoDirectionAndValue a = 
    let splitted = words a
        direction = head splitted
        value = strToInt(last splitted)
    in (direction, value)

getValue :: ([Char], Int) -> [Int]
getValue (direction, value)
    | direction == "forward" = [0, value]
    | direction == "up" = [-value, 0]
    | direction == "down" = [value, 0] 
    | otherwise = error ("Unknown direction " ++ direction)

strToInt str = read str :: Int