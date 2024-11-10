module Lab5Tests exposing (..)

import Lab5 exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)

describe "chunksTests"
    [ 
        test "Take list splittable in equal chunks return list of equal chunks" <|
        \_ -> Expect.equal [[1, 2], [3, 4], [5, 6]] (chunks 2 [1, 2, 3, 4, 5, 6])
    ]