module Documentation exposing (..)

{-| Returns all the ends (tails) of a list

    tails [1, 2, 3] --> [[1, 2, 3], [2, 3], [3], []]
-}
tails : List a -> List (List a)
tails l =
    case l of
        [] -> [[]]
        x::xs -> (x::xs)::tails xs


{-| Returns all combinations for given list of elements.
Combinations are **not** returned in lexicographic order.

```
    combinations [1, 2] --> [[1, 2], [1], [2], []]
    combinations [1, 2, 3] --> [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
```

-}
combinations : List a -> List (List a)
combinations list =
    let 
        helper lst = 
            case lst of
                [] -> [ [] ]
                x :: xs ->
                    let
                        restCombinations = helper xs
                        appx = List.map (\comb -> x :: comb) restCombinations
                    in
                        restCombinations ++ appx
    in
        List.reverse (helper list)


    
