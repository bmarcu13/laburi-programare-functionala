module VerifyExamples.Documentation.Combinations1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Documentation exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#combinations: \n\n    combinations [1, 2]\n    --> [[1, 2], [1], [2], []]" <|
        \() ->
            Expect.equal
                (
                combinations [1, 2]
                )
                (
                [[1, 2], [1], [2], []]
                )
