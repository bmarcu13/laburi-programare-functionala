module Types exposing (..)

type ShapeRec
    = CircleRec {radius: Float}
    | RectangleRec {width: Float, height: Float}
    | TriangleRec {sideA: Float, sideB: Float, sideC: Float}

type alias Point = {x: Float, y: Float}

type Shape2D 
    = Circle {center: Point, radius: Float}
    | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
    | Triangle {pointA: Point, pointB: Point, pointC: Point}

heron a b c =
    let 
        s = (a + b + c) / 2
    in
        sqrt(s * (s - a) * (s - b) * (s - c))

areaRec: ShapeRec -> Float
areaRec shape =
    case shape of
        CircleRec {radius} -> pi * radius * radius
        RectangleRec {width, height} -> width * height
        TriangleRec {sideA, sideB, sideC} -> heron sideA sideB sideC 

pow2 a = a * a

dist: Point -> Point -> Float
dist p1 p2 =
    sqrt (pow2 (p1.x - p2.x) + pow2(p1.y - p2.y))

pointInShape: Point -> Shape2D -> Bool
pointInShape point shape =
    case shape of 
        Circle circle -> dist circle.center point <= circle.radius
        Rectangle {topLeftCorner, bottomRightCorner} -> 
            let 
                minX = min topLeftCorner.x bottomRightCorner.x
                maxX = max topLeftCorner.x bottomRightCorner.x
                minY = min topLeftCorner.y bottomRightCorner.y
                maxY = max topLeftCorner.y bottomRightCorner.y
            in
                point.x >= minX && point.x <= maxX &&
                point.y >= minY && point.y <= maxY
        Triangle {pointA, pointB, pointC} -> 
            let
                a = pointA
                b = pointB
                c = pointC
                
                ab = dist a b
                ac = dist a c
                bc = dist b c
                ap = dist point a
                bp = dist point b
                cp = dist point c
            in
                heron ab ac bc == heron ap ab bp + heron ap ac cp + heron bp bc cp

type Dice = One 
          | Two 
          | Three 
          | Four 
          | Five 
          | Six

type DicePair = DicePair Dice Dice

type alias DicePairAlias = {firstDice: Dice, secondDice: Dice}

luckyRoll: DicePair -> String
luckyRoll (DicePair dice1 dice2) =
    case (dice1, dice2) of
        (Six, Six) -> "VeryLuck"
        (_, Six) -> "Lucky"
        (Six, _) -> "Lucky"
        (_, _) -> "Meh"

