module Test.Generated.Main exposing (main)

import DateTests
import Example
import ExerciseTests
import FirstTest
import Lab5Tests
import OrganizedTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 257406420834573
        , processes = 8
        , globs =
            []
        , paths =
            [ "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\DateTests.elm"
            , "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\Example.elm"
            , "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\ExerciseTests.elm"
            , "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\FirstTest.elm"
            , "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\Lab5Tests.elm"
            , "D:\\Faculta\\An 3\\PF\\Lab\\Lab5\\tests\\OrganizedTests.elm"
            ]
        }
        [ ( "DateTests"
          , [ Test.Runner.Node.check DateTests.suite
            ]
          )
        , ( "Example"
          , [ Test.Runner.Node.check Example.suite
            ]
          )
        , ( "ExerciseTests"
          , [ Test.Runner.Node.check ExerciseTests.suite
            ]
          )
        , ( "FirstTest"
          , [ Test.Runner.Node.check FirstTest.emptyListTakeTest
            ]
          )
        , ( "Lab5Tests"
          , []
          )
        , ( "OrganizedTests"
          , [ Test.Runner.Node.check OrganizedTests.listTests
            ]
          )
        ]