module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)

singleton :: forall arg. arg -> List arg
singleton arg = arg : Nil

null :: forall arg. List arg -> Boolean
null Nil = true
null _ = false

reverse :: forall arg. List arg -> List arg
reverse = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

snoc :: forall arg. List arg -> arg -> List arg
snoc lst arg = reverse (arg : reverse lst)

-- | The Maybe type is used to represent optional values and
-- | can be seen as something like a type-safe null,
-- | where Nothing is null and Just x is the non-null value x

tail :: forall arg. List arg -> Maybe (List arg)
tail Nil = Nothing
tail (_ : lst) = Just lst

-- | fromMaybe x Nothing == x
-- | fromMaybe x (Just y) == y

length :: forall arg. List arg -> Int
length lst =
  if null lst
    then 0
    else 1 + (length ( fromMaybe Nil (tail lst)))

arg1 :: Int
arg1 = 28

list2 :: List Int
list2 = 28 : 29 : Nil

list2empty :: List Int
list2empty = Nil

list3 :: List Int
list3 = 28 : 29 : Nil

arg3 :: Int
arg3 = 30

list4 :: List Int
list4 = 28 : 29 : 30 : Nil

list4empty :: List Int
list4empty = Nil

main :: Effect Unit
main = do
  log ("Result 1: " <> show (singleton arg1))
  log ("Result 2 (full): " <> show (null list2))
  log ("Result 2 (empty): " <> show (null list2empty))
  log ("Result 3: " <> show (snoc list3 arg3))
  log ("Result 4 (full): " <> show (length list4))
  log ("Result 4 (empty): " <> show (length list4empty))