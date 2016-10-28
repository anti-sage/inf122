import Data.Array
import Data.List
import Data.Char

newtype Config = Config (Array (Int, Int) Life)
    deriving (Show)
data Life = Alive | Dead
    deriving (Show, Eq)

showConfig :: Config -> String
showConfig conf@(Config arr) = intercalate "\n" $ map (showLine conf) [0..(snd $ snd $ bounds arr)]

showLine :: Config -> Int -> String
showLine (Config arr) y = map (\ x -> if (arr ! (x, y)) == Dead then '-' else '0') [0..(fst $ snd $ bounds arr)]

--- PARSE

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
parseLife ('D':'e':'a':'d':r) = (Dead, r)
parseLife ('A':'l':'i':'v':'e':r) = (Alive, r)
parseLife _ = error "Parse error, expected dead or alive"

parseTuple :: String -> ((Int, Int), String)
parseTuple str = ((read x, read y), tail $ dropWhile isDigit $ tail $ dropWhile isDigit $ tail str)
    where
        x = takeWhile isDigit $ tail str
        y = takeWhile isDigit $ tail $ dropWhile isDigit $ tail str

--- GAME

step :: Config -> Config
step conf@(Config arr) = Config $ array (bounds arr) $ concat $ map (stepLine conf) [0..(snd $ snd $ bounds arr)]

stepLine :: Config -> Int -> [((Int, Int), Life)]
stepLine (Config arr) y = map (\ x -> ((x, y), stepCell (Config arr) (x, y))) $ [0..(fst $ snd $ bounds arr)]

stepCell :: Config -> (Int, Int) -> Life
stepCell (Config arr) (x, y) = if (arr ! (x, y)) == Alive
                                   then if aliveNeighbors < 2 || aliveNeighbors > 3 then Dead else Alive
                                   else if aliveNeighbors == 3 then Alive else Dead
    where
        aliveNeighbors = countNeighbors (Config arr) (x, y)

countNeighbors :: Config -> (Int, Int) -> Int
countNeighbors (Config arr) (x, y) =
    length . filter (==True) $ (x < maxX && (arr ! (x+1, y)) == Alive) :
                               (x > 0 && (arr ! (x-1, y)) == Alive) :
                               (y < maxY && (arr ! (x, y+1)) == Alive) :
                               (y > 0 && (arr ! (x, y-1)) == Alive) :
                               (x < maxX && y < maxY && (arr ! (x+1, y+1)) == Alive) :
                               (x > 0 && y < maxY && (arr ! (x-1, y+1)) == Alive) :
                               (y > 0 && x < maxX && (arr ! (x+1, y-1)) == Alive) :
                               (y > 0 && x > 0 && (arr ! (x-1, y-1)) == Alive) : []
    where
        maxX = fst $ snd $ bounds arr
        maxY = snd $ snd $ bounds arr

--- MAIN

main :: IO ()
main = do
    putStrLn "Input the size of the board in the form of (m,n)"
    sizeStr <- getLine
    let size = parseSize sizeStr
    putStrLn "Input the initial configuration of the board"
    boardStr <- getLine
    let board = parseBoard boardStr
    let config = (Config (array ((0, 0), size) board))
    putStrLn $ showConfig config
    askForStep config

askForStep :: Config -> IO ()
askForStep config = do
    line <- getLine
    if line == "q"
        then putStr ""
        else do
            let next = step config
            putStrLn $ showConfig next
            askForStep next