

main = do
    contents <- getContents

    let contentList = lines contents

    let splitted = map splitIntoDirectionAndValue contentList
        collected = foldl getValue (0, 0, 0) splitted
        multiplied = computeResult collected

    print collected
    print multiplied

    return ()

computeResult (a,b,c) = b * c

splitIntoDirectionAndValue :: String -> (String, Int)
splitIntoDirectionAndValue a = 
    let splitted = words a
        direction = head splitted
        value = strToInt(last splitted)
    in (direction, value)

getValue ::  (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
getValue (aim, horizontal, depth) (direction, value)
    | direction == "down" =  (aim + value, horizontal, depth)
    | direction == "up" = (aim - value, horizontal, depth)
    | direction == "forward" = (aim, horizontal + value, depth + aim * value)
    | otherwise = error ("Unknown direction " ++ direction)

strToInt str = read str :: Int
