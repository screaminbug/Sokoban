{-# LANGUAGE ScopedTypeVariables #-}
module Graph where

import ListUtil

isGraphClosed :: forall a . Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go [] [initial]
  where
    go ::  [a] -> [a] -> Bool
    go _ []                = True
    go visited (n:ns)
      | elemList n visited = go visited ns
      | isOk n             = go (appendList visited [n]) $ appendList ns (adjacent n)
      | otherwise          = False
      
    

