module Lab5 exposing (..)

import List exposing (..)
import Exercises exposing (..)
import Theme exposing (..)

countVowels l = 
    let 
        isVowel c vowels = 
            case vowels of
                [] -> False
                x::xs -> if c == x then True else isVowel c xs
    in
        l 
        |> String.toList
        |> map (\x -> if isVowel x (String.toList "aeiouAEIOU") then 1 else 0 )
        |> foldl (+) 0

changePreferenceToDarkTheme2 = map (\x -> {x | preferredTheme = Dark})

usersWithPhoneNumbers l = 
    l 
    |> filter (\u -> case u.details.phoneNumber of
        Just _ -> True
        Nothing -> False
    )
    |> map (\u -> u.email)

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