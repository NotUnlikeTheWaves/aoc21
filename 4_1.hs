import Data.Char(digitToInt)
import Data.Bits(shiftL)

main = do
    contents <- readFile "4.txt"
    
    let rawLines = lines contents :: [String]
        rawLinesNoEmpty = filter (/= "") rawLines
        calledNumbers = head rawLinesNoEmpty
        rawCards = tail rawLinesNoEmpty
        bingoCards = createBingoCards rawCards
    return ()


createBingoCards :: [String] -> [[[(Int, Bool)]]]
createBingoCards [] = []
createBingoCards cards = createBingoCard (take 5 cards) : createBingoCards (drop 5 cards)

createBingoCard :: [String] -> [[(Int, Bool)]]
createBingoCard = map (\x -> createBingoCardRow (words x))

createBingoCardRow :: [String] -> [(Int, Bool)]
createBingoCardRow = map (\x -> (read x :: Int, False))

validateColumn :: [[(Int, Bool)]] -> Bool
validateColumn [] = False
validateColumn card = map (\row -> snd (row !! 2)) card
    -- let colCheck = \row -> snd (row !! column) :: [(Int, Bool)] -> Bool
    -- in  map colCheck card || validateColumn card column + 1


validateRow :: [(Int, Bool)] -> Bool
validateRow = all snd

findStuff :: [[Int]] -> Int -> (Int -> Int -> Int) -> [Int]
findStuff a 13 _ = error ("hey " ++ show a) -- a keeps being empty list [] here, so probably too much is filtered?
findStuff [a] _ _ = a
findStuff bitStrings pos decider =
    let amountOfOnes = foldl (\acc l -> acc + (l !! pos)) 0 bitStrings
        amountOfZeros = length bitStrings - amountOfOnes
        digit = decider amountOfOnes amountOfZeros

        filtered = filter (\x -> (x !! pos) == digit) bitStrings
    in findStuff filtered (pos + 1) decider

bitsToInt :: [Int] -> Int
bitsToInt = foldl (\acc v -> acc `shiftL` 1 + v) 0

getBits :: [Char] -> [Int]
getBits = map digitToInt

zipBits = zipWith (+)
