module XvsO.TUI.Client
  ( getPosition
  , askReplay
  , showResult
  ) where

import Data.List (intercalate)
import System.IO (stdout, hFlush, stdin, hReady, BufferMode(NoBuffering), hSetBuffering, hSetEcho)

import XvsO.Model
import XvsO.Utils

printBoard :: (Show value) => Position -> Board value -> IO ()
printBoard (tRow, tColumn) (Board board) = do
  let strBoard = show <$$> board
  let cellLength = maximum (length <$> concat strBoard) + 6
  printLines cellLength strBoard
  where
    printLines :: Int -> [[String]] -> IO ()
    printLines _ [] = return ()
    printLines cellLength [row] = do
      printTop     (length row) cellLength
      printBlanks  (length row) cellLength
      printRow             row  cellLength
      printBlanks_ (length row) cellLength
      printBottom  (length row) cellLength
    printLines cellLength rows = do
      printTop (length $ head rows) cellLength
      printLines_ 0 cellLength rows
    
    printLines_ :: Int -> Int -> [[String]] -> IO ()
    printLines_ _ _ [] = return ()
    printLines_ currentRow cellLength (row:rows) = do
      printBlanks            (length row) cellLength
      printRow                       row  cellLength
      if currentRow == tRow
      then printBlanks_      (length row) cellLength
      else printBlanks       (length row) cellLength
      if null rows
      then printBottom       (length row) cellLength
      else printRowSeparator (length row) cellLength
      printLines_       (succ currentRow) cellLength rows
    
    
    rowsSeparator :: String -> String -> String -> Char -> Int -> Int -> String
    rowsSeparator prefix cellSeparator suffix blank cellsCount cellLength =
      prefix ++
      intercalate cellSeparator (replicate cellsCount $ replicate cellLength blank) ++
      suffix
    
    printTop :: Int -> Int -> IO ()
    printTop = putStrLn ... rowsSeparator "┏" "┳" "┓" '━'
    
    printBottom :: Int -> Int -> IO ()
    printBottom = putStrLn ... rowsSeparator "┗" "┻" "┛" '━'
    
    printRowSeparator :: Int -> Int -> IO ()
    printRowSeparator = putStrLn ... rowsSeparator "┣" "╋" "┫" '━'
    
    printBlanks :: Int -> Int -> IO ()
    printBlanks = putStrLn ... rowsSeparator "┃" "┃" "┃" ' '
    
    printBlanks_ :: Int -> Int -> IO ()
    printBlanks_ cellsCount cellLength =
      putStrLn $
        setCursor cellLength $
          rowsSeparator "┃" "┃" "┃" ' ' cellsCount cellLength
    
    printRow :: [String] -> Int -> IO ()
    printRow row cellLength =
      putStrLn $ "┃" ++ intercalate "┃" (addSpaces cellLength <$> row) ++ "┃"
    
    addSpaces :: Int -> String -> String
    addSpaces l s = do
          let needed = l - length s
          let halfNeeded = needed `div` 2
          let prefix = replicate halfNeeded ' '
          let suffix = replicate (halfNeeded + (needed `mod` 2)) ' '
          prefix ++ s ++ suffix
    
    setCursor :: Int -> String -> String
    setCursor cellLength str = do
      let position = tColumn * (cellLength + 1) + cellLength `div` 2
      if odd cellLength
      then setAt position '^' str
      else setAt position '^' $ setAt (position + 1) '^' str

getKey :: IO [Char]
getKey = reverse <$> getKey_ ""
  where getKey_ chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey_ else return) (char:chars)

getPosition :: Show value => Int -> value -> Board value -> IO Position
getPosition step value wBoard@(Board board) = getPosition_ (0, 0)
  where
    getPosition_ :: Position -> IO Position
    getPosition_ position = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      putStrLn $ "Step: " ++ show (succ step)
      putStrLn $ "You are: " ++ show value
      printBoard position wBoard
      hFlush stdout
      key <- getKey
      case key of
        "\ESC[A" -> getPosition_ $ positionUp    position
        "\ESC[B" -> getPosition_ $ positionDown  position
        "\ESC[C" -> getPosition_ $ positionRight position
        "\ESC[D" -> getPosition_ $ positionLeft  position
        "\n"     -> return                       position
        _        -> getPosition_                 position
    
    positionUp :: Position -> Position
    positionUp (row, column) =
      (if row == 0 then 0 else pred row, column)
    
    positionDown :: Position -> Position
    positionDown (row, column) =
      (if succ row == length board then row else succ row, column)
    
    positionLeft :: Position -> Position
    positionLeft (row, column) =
      (row, if column == 0 then 0 else pred column)

    positionRight :: Position -> Position
    positionRight (row, column) =
      (row, if succ column == length (board !! row) then column else succ column)

askReplay :: IO Bool
askReplay = askReplay_ True
  where
    askReplay_ :: Bool -> IO Bool
    askReplay_ choose = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      putStrLn      "Do you whant repeat game?"
      putStrLn      "    YES      NO"
      if choose
      then putStrLn "    ^^^"
      else putStrLn "             ^^"
      hFlush stdout
      key <- getKey
      case key of
        "\ESC[C" -> askReplay_ False
        "\ESC[D" -> askReplay_ True
        "\n"     -> return     choose
        "y"      -> return     True
        "n"      -> return     False
        _        -> askReplay_ choose

showResult :: GameResult -> IO ()
showResult result = do
  putStrLn $
    case result of
      Win    -> "Congrutilations!"
      Loose  -> "You could try again!"
      Mirror -> "No winner, no looser!"
  hFlush stdout
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  _ <- getKey
  return ()
  