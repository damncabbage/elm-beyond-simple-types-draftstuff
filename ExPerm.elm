module ExPerm exposing (..)

import List


permutations : List a -> List (List a)
permutations input =
  case input of
    [] ->
      [[]]
    items ->
      let
        one (x,xs) =
          List.map (\i -> x :: i) (permutations xs)
      in
        List.concatMap one (select items)

-- eg. [1,2,3] => [ (1,[2,3]), (2,[1,3]), (3,[1,2]) ]
select : List a -> List (a, List a)
select things =
  case things of
    [] -> []
    (x :: xs) ->
      (x, xs) :: (List.map (\(y,ys) -> (y, x :: ys)) (select xs))


-- let three = do
--   let elements = [1,2,3]
--   x <- elements
--   y <- elements
--   z <- elements
--   pure [x,y,z]

three : List a -> List (List a)
three elements =
  List.concatMap (\x ->
    List.concatMap (\y ->
      List.concatMap (\z ->
        [ [x, y, z] ]
      ) elements
    ) elements
  ) elements



-- let listReplicate times elements =
--   replicateM times elements

listReplicate : Int -> List a -> List (List a)
listReplicate times elements =
  let
    go count =
      if count <= 0
        then
          [[]]
        else
          listLift2 (\a b -> a :: b)
            elements
            (go (count - 1))
  in
    go times


listApply fab xs =
  case fab of
    [] -> []
    (f :: fs) ->
      List.concat [(List.map f xs), (listApply fs xs)]

listLift2 f a b =
  listApply (List.map f a) b
