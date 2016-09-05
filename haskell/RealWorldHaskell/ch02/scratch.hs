-- random question answers and snippets from chapter 2

{-
lastButOne
  | [] = error "lastButOne called on an empty list"
  | [x] = x
  | (x:xs) = lastButOne xs
-}
lastButOne (x:xs) = if length xs == 1
                    then x
                    else lastButOne xs
