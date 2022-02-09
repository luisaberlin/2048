module ArraySort
(
   rowAction,
) where

sortRow :: (Eq a, Num a) => [a] -> [a]
sortRow arr
    | head arr /= 0 && arr !! 1 == 0 = sortRow (0 : head arr : arr !! 2 : [arr !! 3])
    | arr !! 1 /= 0 && arr !! 2 == 0 = sortRow (head arr : 0 : arr !! 1 : [arr !! 3])
    | arr !! 2 /= 0 && arr !! 3 == 0 = sortRow (head arr : arr !! 1 : 0 : [arr !! 2])
    | otherwise = arr

sumUpNumbers :: (Eq a, Num a) => [a] -> [a]
sumUpNumbers [] = []
sumUpNumbers [x] = [x]
sumUpNumbers arr =
    if last arr == last (init arr)
        then sumUpNumbers (init (init arr)) ++ [0] ++ [last arr + last (init arr)]
        else sumUpNumbers (init arr) ++ [last arr]


rowAction :: (Eq a, Num a) => [a] -> [a]
rowAction arr = sortRow $ (sumUpNumbers . sortRow) arr


-- Testing
a = rowAction [2,0,2,2] == [0,0,2,4]
b = rowAction [2,2,2,2] == [0,0,4,4]
c = rowAction [8,0,8,0] == [0,0,0,16]
d = rowAction [0,16,0,4] == [0,0,16,4]
e = rowAction [2,2,2,4] == [0,2,4,4]

main :: IO ()
main =
    putStrLn ("Test A " ++ (if a then "✅" else "❌"))
    >> putStrLn ("Test B " ++ (if b then "✅" else "❌"))
    >> putStrLn ("Test C " ++ (if c then "✅" else "❌"))
    >> putStrLn ("Test D " ++ (if d then "✅" else "❌"))
    >> putStrLn ("Test E " ++ (if e then "✅" else "❌"))