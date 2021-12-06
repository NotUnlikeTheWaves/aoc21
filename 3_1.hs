import Data.Char(digitToInt)
import Data.Bits(shiftL)

main = do
    contents <- getContents

    let contentList = lines contents

    let bitStrings = map getBits contentList
        accumulated = foldl zipBits (head bitStrings) (init bitStrings)
        gamma = getGammaRate accumulated (length contentList)
        epsilon = map (\x -> if x == 0 then 1 else 0) gamma
        gammaInteger = bitsToInt gamma
        epsilonInteger = bitsToInt epsilon
        
    print gamma
    print (gammaInteger, epsilonInteger)
    
    print ("Result is " ++ show (gammaInteger * epsilonInteger))
    return ()


bitsToInt = foldl (\acc v -> acc `shiftL` 1 + v) 0

getBits :: [Char] -> [Int]
getBits = map digitToInt

zipBits = zipWith (+)

getGammaRate ::  [Int] -> Int -> [Int]
getGammaRate items length = map (\x -> if x >= length `div` 2 then 1 else 0) items
