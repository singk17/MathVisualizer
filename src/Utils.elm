module Utils exposing (..)
import Html exposing (a)
import Html exposing (b)
import Html exposing (li)

clamp : comparable -> comparable -> comparable -> comparable
clamp mi ma v = max mi v |> min ma

actHead : List a -> a
actHead li = case (List.head li) of
  Just a -> a
  Nothing -> Debug.todo "Empty list given to head"

actTail : List a -> List a
actTail li = case (List.tail li) of
  Just a -> a
  Nothing -> Debug.todo "Empty list given to tail"

actMin : List comparable -> comparable
actMin li = case (List.minimum li) of
  Just a -> a
  Nothing -> Debug.todo "empty list given to minimum"

actMax : List comparable -> comparable
actMax li = case (List.maximum li) of
  Just a -> a
  Nothing -> Debug.todo "empty list given to maximum"

lzip : List a -> List b -> List (a,b)
lzip a b = List.map2 Tuple.pair a b

llzip : List a -> List a -> List (List a)
llzip a b = List.map2 (\x y -> [x,y]) a b

flatten : List (a,a) -> List a
flatten li = List.foldr (\(b,c) a -> a ++ [b,c]) [] li

flatten2D : List (List a) -> List a
flatten2D li = List.foldr (++) [] li

filUnique : List a -> List a
filUnique li = List.foldr (\v o -> if (List.member v o) then o else o ++ [v]) [] li

removeAt : Int -> List a -> List a
removeAt a li = (List.take a li) ++ (List.drop (a+1) li)

elem : Int -> List a -> a
elem a li = case ( List.drop a li |> List.head ) of
              Just i -> i
              Nothing -> Debug.todo <| (++) "Index out of bounds " <| String.fromInt a

updateAt : Int -> (a -> a) -> List a -> List a
updateAt a upd li = (List.take a li) ++ [ upd <| elem a li ] ++ (List.drop (a+1) li)

toFloat : String -> Float
toFloat s =
  case (String.toFloat s) of
    Just a -> a
    Nothing -> Debug.todo <| s ++ " was not a float"

toInt : String -> Int
toInt s =
  case (String.toInt s) of
    Just a -> a
    Nothing -> Debug.todo <| s ++ " was not a integer"

enforceJust : Maybe a -> a
enforceJust ma =
  case ma of
      Just a -> a
      Nothing -> Debug.todo "Nothing given to enforce Just"
