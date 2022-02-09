-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html

-- Other modules
import System.Random

-- Own modules
import ArraySort
import Matrix
import GHC.IO.Handle
import GHC.IO.Handle.FD

handlePressedKey :: [MatrixElement] -> Char -> [MatrixElement]
handlePressedKey m k
    | k == 'j' = moveLeft m
    | k == 'l' = moveRight m
    | k == 'k' = moveDown m
    | k == 'i' = moveUp m
    | otherwise = m
    

-- Check if matrix changed, if so add on a random field a 2
addRandomIfMatrixChanged :: [MatrixElement] -> [MatrixElement] -> IO [MatrixElement]
addRandomIfMatrixChanged old new = if old /= new then addAtRandomField new else return new

-- Add 2 to a random field and returns new matrix as IO
addAtRandomField :: [MatrixElement] -> IO [MatrixElement]
addAtRandomField m = randomRIO (0, length (getEmtyFields m)-1 :: Int)
    >>= \r -> return (getEmtyFields m !! r)
    >>= \randomField -> return (swapElement m (row randomField) (column randomField) 2)


-- Checks if user lost, won or if the game will continue
-- If it will continue it returns the matrix with a newly added 2 to a random field
checkIfEnd :: [MatrixElement] -> [MatrixElement] -> IO ()
checkIfEnd oldMatrix newMatrix
    | null (getEmtyFields newMatrix) = printMatrix newMatrix >> putStrLn "\n❌ You lost. ❌"
    | checkIfWon newMatrix = printMatrix newMatrix >> putStrLn "\n🎉 Congratulations! You won. 🎉"
    | otherwise = addRandomIfMatrixChanged oldMatrix newMatrix >>= \matrix -> mainLoop matrix



-- Gets user input and checks if valid. If not valid get another user input
getKey :: IO Char
getKey = hSetBuffering stdin NoBuffering >> getChar >>= \k -> if notValidKey k then getKey else return k

notValidKey :: Char -> Bool
notValidKey k = k /= 'j' && k /= 'l' && k /= 'k' && k /= 'i'



-- Prints matrix, gets user input, and handles input
mainLoop :: [MatrixElement] -> IO ()
mainLoop matrix = printMatrix matrix
    >> getKey >>= \k -> putStrLn "" >> checkIfEnd matrix (handlePressedKey matrix k) 


main :: IO ()
main = putStrLn "Welcome to 2048."
    >> putStrLn "\nPress these keys to play."
    >> putStrLn "i Up"
    >> putStrLn "k Down"
    >> putStrLn "j Left"
    >> putStrLn "l Right"
    >> putStrLn "\nLet's start!"
    >> addAtRandomField newMatrix -- Add number to random a field of new matrix
    >>= addAtRandomField -- Add number to random a field of the matrix
    >>= mainLoop -- Call mainLoop with new matrix