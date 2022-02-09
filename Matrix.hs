module Matrix where

import ArraySort


data MatrixElement = MatrixElement { row :: Int
                     , column :: Int
                     , value :: Int } deriving (Show, Eq)

newMatrix :: [MatrixElement]
newMatrix = [MatrixElement row column 0 | row <- [0..3], column <- [0..3]]


-- Get
getRow :: [MatrixElement] -> Int -> [MatrixElement]
getRow m i = filter (\e -> row e == i) m

getColumn :: [MatrixElement] -> Int -> [MatrixElement]
getColumn m i = filter (\e -> column e == i) m


-- Swap
swapElement :: [MatrixElement] -> Int -> Int -> Int -> [MatrixElement]
swapElement m row column newValue = x ++ MatrixElement row column newValue : ys
    where index = row * 4 + column
          (x,_:ys) = splitAt index m


-- Get Array with all empty MatrixElements of an list of MatrixElements
getEmtyFields :: [MatrixElement] -> [MatrixElement]
getEmtyFields m = filter (\x -> value x == 0) m


-- Check if array of MatrixElements contains 2048
checkIfWon :: [MatrixElement] -> Bool
checkIfWon m = any (\x -> value x == 2048) m


-- As array
rowAsArray :: [MatrixElement] -> Int -> [Int]
rowAsArray m i  = [value me | me <- getRow m i]

columnAsArray :: [MatrixElement] -> Int -> [Int]
columnAsArray m i  = [value me | me <- getColumn m i]

matrixAsArray :: [MatrixElement] -> [[Int]]
matrixAsArray m = [rowAsArray m 0, rowAsArray m 1, rowAsArray m 2, rowAsArray m 3]


-- Insert 
insertRow :: [MatrixElement] -> [Int] -> Int -> [MatrixElement]
insertRow m arr index = [e | x <- m, let e = newRowElementIfNeeded index x (arr !! column x)]

-- insertColumn :: [MatrixElement] -> [Int] -> Int -> [MatrixElement]
-- insertColumn m arr index = [e | x <- m, let e = newColumnElementIfNeeded index x (arr !! row x)]

newRowElementIfNeeded :: Int -> MatrixElement -> Int -> MatrixElement
newRowElementIfNeeded rowIndex element value
    | rowIndex == row element = MatrixElement (row element) (column element) value
    | otherwise = element

-- newColumnElementIfNeeded :: Int -> MatrixElement -> Int -> MatrixElement
-- newColumnElementIfNeeded columnIndex element value
--     | columnIndex == column element = MatrixElement (row element) (column element) value
--     | otherwise = element

insertRows :: [[Int]] -> [MatrixElement] -> Int -> [MatrixElement]
insertRows rows m i
    | i < 4 = insertRows rows (insertRow m (rows !! i) i) (i+1)
    | otherwise = m


-- Sort matrix
getMovedRightAsArrays :: [MatrixElement] -> [[Int]]
getMovedRightAsArrays m = [newArr | arr <- matrixAsArray m, let newArr = rowAction arr]

getMovedLeftAsArrays :: [MatrixElement] -> [[Int]]
getMovedLeftAsArrays m = [newArr | arr <- matrixAsArray m, let newArr = (reverse . rowAction) $ reverse arr]



-- Move commands 
moveRight :: [MatrixElement] -> [MatrixElement]
moveRight m = insertRows (getMovedRightAsArrays m) m 0

moveLeft :: [MatrixElement] -> [MatrixElement]
moveLeft m = insertRows (getMovedLeftAsArrays m) m 0

moveUp :: [MatrixElement] -> [MatrixElement]
moveUp m = rotate (moveLeft (rotate m))

moveDown :: [MatrixElement] -> [MatrixElement]
moveDown m = rotate (moveRight (rotate m))


-- Rotate matrix (switch column and row)
switchColumnWithRow :: MatrixElement -> MatrixElement
switchColumnWithRow e = MatrixElement (column e) (row e) (value e)

--     (First call: rotate right; second call: rotate left) (actually also mirrors)
rotate :: [MatrixElement] -> [MatrixElement]
rotate m = [newE | e <- m, let newE = switchColumnWithRow e]


-- Print
printMatrix :: [MatrixElement] -> IO ()
printMatrix m = putStrLn "_____________________________"
    >> putStrLn "|      |      |      |      |"
    >> putStrLn (arrAsText $ rowAsArray m 0)
    >> putStrLn "|      |      |      |      |"
    >> putStrLn (arrAsText $ rowAsArray m 1)
    >> putStrLn "|      |      |      |      |"
    >> putStrLn (arrAsText $ rowAsArray m 2)
    >> putStrLn "|      |      |      |      |"
    >> putStrLn (arrAsText $ rowAsArray m 3)
    >> putStrLn "|      |      |      |      |"
    >> putStrLn "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾"

arrAsText :: [Int] -> String
arrAsText arr = foldl (\acc y -> acc ++ (addSpaces y) ++ " | ") "| " arr

addSpaces :: Int -> String
addSpaces n
    | length (show n) == 1 = "   " ++ value
    | length (show n) == 2 = "  " ++ value
    | length (show n) == 3 = " " ++ value
    | otherwise = show n
    where value = if n == 0 then " " else show n -- do not show zeros (--> better UIj)


-- Testing

testEmpty = newMatrix
oneRow = insertRow testEmpty [0,0,4,0] 0
twoRows = insertRow oneRow [0,2,0,0] 1
threeRows = insertRow twoRows [0,2,0,32] 2
fourRows = insertRow threeRows [8,2,4,8] 3