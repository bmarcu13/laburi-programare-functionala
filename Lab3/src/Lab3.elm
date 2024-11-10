module Lab3 exposing (..)

import Shape exposing (..)

safeDiv : Int -> Int -> Maybe Int
safeDiv a b = 
    case b of
        0 -> Nothing
        _ -> Just (a // b)

lenTail : List a -> Int
lenTail list = 
    let 
        lenAcc l acc =
            case l of
                [] -> acc
                _::xs -> lenAcc xs (acc + 1)
    in
        lenAcc list 0

last : List a -> Maybe a
last list = 
    let
        lastAcc l lastElem =
            case l of
                [] -> lastElem
                x::xs -> lastAcc xs x
    in
        case list of
            [] -> Nothing
            x::xs -> Just (lastAcc xs x)

index : Int -> List a -> Maybe a
index ind list =
    let 
        indexAcc i l acc = 
            case l of
                [] -> Nothing
                x::xs -> if i == acc then Just x else indexAcc i xs (acc + 1)
    in
        case list of 
            [] -> Nothing
            _::_ -> indexAcc ind list 0

fibs start end =
    let 
        fibsAcc s e crt a b listAcc =
            if crt == e then Just listAcc
            else if crt < s then
                fibsAcc s e (crt + 1) b (a + b) listAcc
            else 
                fibsAcc s e (crt + 1) b (a + b) (listAcc ++ [b])
    in
        if  start >= end then Nothing 
        else fibsAcc start end 0 0 1 []

cmpShapes : Shape -> Shape -> Result String Order
cmpShapes shape1 shape2 =
    case (safeArea shape1, safeArea shape2) of
        (Err e, _) -> Err ("Invalid input for left shape: " ++ e)
        (_, Err e) -> Err ("Invalid input for right shape: " ++ e)
        (Ok a1, Ok a2) -> if a1 < a2 then Ok LT else if a1 == a2 then Ok EQ else Ok GT 

totalArea : List Shape -> Result (Int, InvalidShapeError) Float
totalArea list =
    let 
        calcAreaAcc l indexAcc sumAcc = 
            case l of 
                [] -> Ok sumAcc
                x::xs -> 
                    case safeAreaEnum x of
                        Err err -> Err (indexAcc, err)
                        Ok area -> calcAreaAcc xs (indexAcc + 1) (sumAcc + area) 
    in
        calcAreaAcc list 0 0
            