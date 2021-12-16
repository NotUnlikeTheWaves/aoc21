import Data.List(transpose)

main = do
    contents <- readFile "4.txt"

    let rawLines = lines contents :: [String]
        rawLinesNoEmpty = filter (/= "") rawLines
        calledNumbers = head rawLinesNoEmpty
        rawCards = tail rawLinesNoEmpty
        bingoCards = createBingoCards rawCards
    print bingoCards
    print calledNumbers
    return ()


createBingoCards :: [String] -> [[[(Int, Bool)]]]
createBingoCards [] = []
createBingoCards cards = createBingoCard (take 5 cards) : createBingoCards (drop 5 cards)

createBingoCard :: [String] -> [[(Int, Bool)]]
createBingoCard = map (createBingoCardRow . words)

createBingoCardRow :: [String] -> [(Int, Bool)]
createBingoCardRow = map (\x -> (read x :: Int, False))

-- bingo :: [[[(Int, Bool)]]] -> [Int] -> (Int, Int)
-- bingo cards numbers = let


checkBoard :: [[(Int, Bool)]] -> Bool
checkBoard board =
    let rows = board
        columns = transpose board
        slices = rows ++ columns
    in any validateSlice slices



validateSlice :: [(Int, Bool)] -> Bool
validateSlice = all snd

applyBoard :: [[(Int, Bool)]] -> Int -> [[(Int, Bool)]]
applyBoard board number = map (map (\(field, called) -> (field, called || number == field))) board



-- ???

-- split :: [Char] -> Char -> [String]
-- split string sep = foldr (\[h:t] v ->
--         if v == sep
--             then ["", h] ++ t
--             else [v ++ h] : t
--     ) [""] string
