import Data.List(transpose)
import Data.Maybe

main = do
    contents <- readFile "4.txt"

    let rawLines = lines contents :: [String]
        rawLinesNoEmpty = filter (/= "") rawLines
        calledNumbers = head rawLinesNoEmpty
        rawCards = tail rawLinesNoEmpty
        bingoCards = createBingoCards rawCards
    print bingoCards
    print calledNumbers
    print (split calledNumbers ',')
    return ()


createBingoCards :: [String] -> [[[(Int, Bool)]]]
createBingoCards [] = []
createBingoCards cards = createBingoCard (take 5 cards) : createBingoCards (drop 5 cards)

createBingoCard :: [String] -> [[(Int, Bool)]]
createBingoCard = map (createBingoCardRow . words)

createBingoCardRow :: [String] -> [(Int, Bool)]
createBingoCardRow = map (\x -> (read x :: Int, False))


play :: [[[(Int, Bool)]]] -> [Int] -> (Int, Int)
play _ [] = (0, 0)
play boards (number:rest) = 
    let newBoards = map (\board -> applyBoard board number) boards
        winner = first checkBoard newBoards
    in if isJust winner then (Number, 100) else play newBoards rest



first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first f (head:tail) = if f head then Just head else first f tail

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


split :: String -> Char -> [String]
split [] _ = []
split str sep =
    let (word, rest) = break (== sep) str
    in word : split (dropWhile (== sep) rest) sep

-- split [] _ = []
-- split [char:rest] sep = 
--     if char /= sep 
--         then 


-- ???

-- split :: [Char] -> Char -> [String]
-- split string sep = foldr (\[h:t] v ->
--         if v == sep
--             then ["", h] ++ t
--             else [v ++ h] : t
--     ) [""] string
