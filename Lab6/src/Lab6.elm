module Lab6 exposing (..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Recipie exposing (..)

r = Recipe "Bostan festiv" "https://www.bostan_festiv.com" ["bostan", "crema bucatarului", "sare", "festivitate"] []

main = recipieView r