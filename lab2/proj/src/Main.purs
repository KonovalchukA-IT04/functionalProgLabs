module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- | Data structures import
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

-- | Test list for this lab (few numbers from the Fibonacci number)
listExample :: List Int
listExample = 0 : 1 : 1 : 2 : 3 : 5 : 8 : 13 : 21 : 34 : 55 : 89 : Nil

-- | Func from prev lab
reverse :: forall arg. List arg -> List arg
reverse list = go Nil list
  where
  go ac Nil = ac
  go ac (el : lst) = go (el : ac) lst


-- | 1) findIndex
findIndex :: forall arg. (arg -> Boolean) -> List arg -> Maybe Int
findIndex predicate list = getEl 0 predicate list
  where
  getEl :: Int -> (arg -> Boolean) -> List arg -> Maybe Int
  getEl num pred (el : lst) =
    if    pred el
    then  Just num
    else  getEl (num + 1) pred lst
  getEl _ _ Nil = Nothing


-- | 2) findLastIndex
findLastIndex :: forall arg. (arg -> Boolean) -> List arg -> Maybe Int
findLastIndex predicate list = getEl 0 predicate list Nothing
  where
  getEl :: Int -> (arg -> Boolean) -> List arg -> Maybe Int -> Maybe Int
  getEl num pred (el : lst) item =
    if    pred el
    then  getEl (num + 1) pred lst (Just num)
    else  getEl (num + 1) pred lst item
  getEl _ _ Nil item = item


-- | 3) zip
zip :: forall arg1 arg2. List arg1 -> List arg2 -> List (Tuple arg1 arg2)
zip (el1 : lst1) (el2 : lst2) = (Tuple el1 el2) : (zip lst1 lst2)
zip _ _ = Nil


-- | 4) unzip
unzip :: forall el1 el2. List (Tuple el1 el2) -> Tuple (List el1) (List el2)
unzip zipLst = Tuple (unzipA zipLst) (unzipB zipLst)
  where
  
  unzipA :: List (Tuple el1 el2) -> List el1
  unzipA (el : lst) = (fst el) : (unzipA lst)
  unzipA Nil = Nil

  unzipB :: List (Tuple el1 el2) -> List el2
  unzipB (el : lst) = (snd el) : (unzipB lst)
  unzipB Nil = Nil


-- | 5) filter
filter :: forall arg. (arg -> Boolean) -> List arg -> List arg
filter pred (el : lst) =
  if    pred el
  then  el : (filter pred lst)
  else  filter pred lst
filter _ Nil = Nil


-- | 6) tail recursion filter
filterTR :: forall arg. (arg -> Boolean) -> List arg -> List arg
filterTR predicate list = tailRec predicate list Nil
  where
  tailRec :: (arg -> Boolean) -> List arg -> List arg -> List arg
  tailRec pred (el : lst) res = 
    if    pred el
    then  tailRec pred lst (el : res)
    else  tailRec pred lst res
  tailRec _ _ res = reverse res


-- | 7) take
take :: forall arg. Int -> List arg -> List arg
take num (el : lst) =
  if    num == 0
  then  Nil
  else  el : (take (num - 1) lst)
take _ _ = Nil


-- | 8) tail recursion take
takeTR :: forall arg. Int -> List arg -> List arg
takeTR number list = tailRec number list Nil
  where
  tailRec :: Int -> List arg -> List arg -> List arg
  tailRec num (el : lst) res =
    if    num == 0
    then  reverse res
    else  tailRec (num - 1) lst ( el : res )
  tailRec _ _ res = reverse res


main :: Effect Unit
main = do
  log ( "List: " <> show( listExample ) )

  log ( "\n _findIndex_" )
  log ( "el = 1: " <> show( findIndex ( \el -> el == 1 ) listExample ) )
  log ( "el < 2: " <> show( findIndex ( \el -> el < 2 ) listExample ) )
  log ( "el = 4: " <> show( findIndex ( \el -> el == 4 ) listExample) )
  
  log ( "\n _findLastIndex_" )
  log ( "el = 1: " <> show( findLastIndex ( \el -> el == 1 ) listExample ) )
  log ( "el < 2: " <> show( findLastIndex ( \el -> el < 2 ) listExample ) )
  log ( "el = 4: " <> show( findLastIndex ( \el -> el == 4 ) listExample ) )

  log ( "\n _zip_" )
  log ( "list + reverse list: \n" <> show( zip (reverse listExample) listExample ) )
  
  log ( "\n _unzip_" )
  log ( show( unzip ( zip (reverse listExample) listExample ) ) )

  log ( "\n _filter_" )
  log ( "el = 1: " <> show( filter ( \el -> el == 1 ) listExample ) )
  log ( "el < 2: " <> show( filter ( \el -> el < 2 ) listExample ) )
  log ( "el > 20: " <> show( filter ( \el -> el > 20 ) listExample ) )
  log ( "el = 4: " <> show( filter ( \el -> el == 4 ) listExample) )

  log ( "\n _tail recursion filter_" )
  log ( "el = 1: " <> show( filterTR ( \el -> el == 1 ) listExample ) )
  log ( "el < 2: " <> show( filterTR ( \el -> el < 2 ) listExample ) )
  log ( "el > 20: " <> show( filterTR ( \el -> el > 20 ) listExample ) )
  log ( "el = 4: " <> show( filterTR ( \el -> el == 4 ) listExample ) )

  log ( "\n _take_" )
  log ( "take 1: " <> show( take 1 listExample ) )
  log ( "take 3: " <> show( take 3 listExample ) )
  log ( "take 20: " <> show( take 20 listExample ) )
  log ( "take 0: " <> show( take 0 listExample) )

  log ( "\n _tail recursion take_" )
  log ( "take 1: " <> show( takeTR 1 listExample ) )
  log ( "take 3: " <> show( takeTR 3 listExample ) )
  log ( "take 20: " <> show( takeTR 20 listExample ) )
  log ( "take 0: " <> show( takeTR 0 listExample) )