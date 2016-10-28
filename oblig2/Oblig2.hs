import Data.Array
import Data.List
import Data.Char

newtype Config = Config (Array (Int, Int) Life)
    deriving (Show)
data Life = Alive | Dead
    deriving (Show, Eq)

showConfig :: Config -> String
showConfig conf@(Config arr) = intercalate "\n" $ map (showLine conf) [0..(fst $ snd $ bounds arr)]

showLine :: Config -> Int -> String
showLine (Config arr) y = map (\ x -> if (arr ! (x, y)) == Dead then '-' else '0') [0..(fst $ snd $ bounds arr)]

parseSize :: String -> (Int, Int)
parseSize str = fst $ parseTuple str

parseBoard :: String -> [((Int, Int), Life)]
parseBoard str = fst $ parseCell $ tail str

parseCell :: String -> ([((Int, Int), Life)], String)
parseCell [] = ([], [])
parseCell str = ((coord, life) : board, r3)
    where
        (coord, r1) = parseTuple $ tail str
        (life, r2) = parseLife $ tail r1
        (board, r3) = parseCell $ drop 2 r2

parseLife :: String -> (Life, String)
parseLife str
    | (take 4 str) == "Dead" = (Dead, drop 4 str)
    | (take 5 str) == "Alive" = (Alive, drop 5 str)
    | otherwise = error "Parse error, expected dead or alive"

parseTuple :: String -> ((Int, Int), String)
parseTuple str = ((read x, read y), tail $ dropWhile isDigit $ tail $ dropWhile isDigit $ tail str)
    where
        x = takeWhile isDigit $ tail str
        y = takeWhile isDigit $ tail $ dropWhile isDigit $ tail str

main :: IO ()
main = do
    putStrLn "Input the size of the board in the form of (m, n)"
    sizeStr <- getLine
    let size = parseSize sizeStr
    putStrLn "Input the initial configuration of the board"
    boardStr <- getLine
    let board = parseBoard boardStr
    let config = (Config (array ((0, 0), size) board))
    putStrLn $ showConfig config