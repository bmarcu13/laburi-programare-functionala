module Lab4 exposing (..)
import List exposing (..)

enumerate : List a -> List (Int, a)
enumerate = 
    let 
        helper indexAcc list = 
            case list of 
                [] -> []
                x::xs -> (indexAcc, x) :: helper (indexAcc + 1) xs
    in
        helper 0

repeat : Int -> a -> List a
repeat n elem =
    if n == 0 then []
    else elem :: repeat (n - 1) elem

countVowels : String -> Int
countVowels s = 
    List.foldl 
        (+)
        0 
        (List.map
            (
                let 
                    helper vowels c =
                        case vowels of
                            [] -> 0
                            x::xs -> if c == x then 1 else helper xs c
                in
                    helper (String.toList "aeiou")
            )
            (String.toList s)
        )

partition : comparable -> List comparable -> (List comparable, List comparable)
partition pivot list = 
    let
        myFilter pred l =
            case l of
                [] -> []
                x::xs -> if pred x then x :: (myFilter pred xs) else myFilter pred xs
    in
        (myFilter (\x -> x < pivot) list, myFilter (\x -> x >= pivot) list)


countriesWithCapital : List (String, String) -> (String -> Bool) -> List (String)
countriesWithCapital list pred =
    case list of
        [] -> []
        (country, capital)::xs -> if pred capital then country :: (countriesWithCapital xs pred) else (countriesWithCapital xs pred)


collect : List (Result err ok) -> Result err (List ok)
collect = 
    let
        helper lAcc l =
            case l of
                [] -> Ok (List.reverse lAcc)
                (Err val)::_ -> Err val
                (Ok val)::xs -> helper (val :: lAcc) xs
    in
        helper []

chunks : Int -> List a -> List (List a)
chunks c list =
    let
        helper cnt chunkAcc lAcc n l =
            case l of
                [] -> if chunkAcc /= [] then chunkAcc :: lAcc else lAcc
                x::xs -> 
                    if cnt < n then 
                        helper (cnt + 1) (x :: chunkAcc) lAcc n xs 
                    else 
                        helper 1 [] (((List.reverse (x :: chunkAcc)) :: lAcc )) n xs
                
    in
        List.reverse (helper 1 [] [] c list)

allFoldl : (a -> Bool) -> List a -> Bool
allFoldl cnd l = List.foldl (\x p -> (cnd x) && p) True l

anyFoldl : (a -> Bool) -> List a -> Bool
anyFoldl cnd l = List.foldl (\x p -> (cnd x) || p) False l

type Month = January 
          | February 
          | March 
          | April 
          | May 
          | June 
          | July 
          | August 
          | September 
          | October 
          | November 
          | December

type alias Date = {day: Int, month: Month, year: Int}

daysInMonth : Month -> Int -> Int
daysInMonth month year =
    let 
        isLeapYear y = (modBy 400 y == 0) || ((modBy 4 y == 0) && not (modBy 100 y == 0))
    in
        if month == February then
            if isLeapYear year then 
                28
            else 
                29
        else if month == January || month == March || month == May || month == July || month == August || month == October || month == December then
            31
        else 
            30

createDate : Int -> Month -> Int -> Maybe Date
createDate d m y =
    let 
        dd = daysInMonth m y
    in
        if y < 1970 || y > 3000 then 
            Nothing
        else if d /= dd then
            Nothing
        else 
            Just {day= dd, month= m, year= y}

filterMap : (a -> Maybe b) -> List a -> List b
filterMap f list =
    let 
        myFilter l =
            case l of 
                [] -> []
                x::xs -> case x of
                    Just e -> e :: (myFilter xs)
                    Nothing -> myFilter xs
    in
        myFilter (map f list)

enumerateFoldlOnly : List a -> List (Int, a)
enumerateFoldlOnly l = 
    let 
        lenFold = foldl (\x acc -> acc + 1) 0
        enum = foldl (\x acc -> acc ++ [(lenFold acc, x)]) []
    in 
        enum l




 
    

