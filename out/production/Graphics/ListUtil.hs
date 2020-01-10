module ListUtil where

elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False
elemList e (x:xs)
 | e == x     = True
 | otherwise  = elemList e xs
                                        

appendList :: [a] -> [a] -> [a]
appendList = (++)

listLength :: [a] -> Int
listLength = length

filterList ::  (a -> Bool) -> [a] -> [a]
filterList = filter

mapList :: (t -> a) -> [t] -> [a]
mapList _ [] =     []
mapList f (c:cs) = f c : mapList f cs

foldAppendList :: (Int -> [a]) -> [a]
foldAppendList something = go (-10)
  where
    go 11 = []
    go n  = appendList (something n) $ go $ n + 1
    
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length chunk' < n = []
  | otherwise         = chunk' : chunk n (tail xs)
  where
    chunk' = take n xs 
