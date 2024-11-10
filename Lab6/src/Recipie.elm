module Recipie exposing (..)
import Html exposing (..)
import Html.Attributes exposing(..)

type alias Recipe =
    { title: String
    , linkToOriginal: String
    , ingredients: List String
    , method: List String
    }

recipieView: Recipe -> Html msg
recipieView recipie =
    let 
        ingredientsList r = ul [] (List.map (\item -> li [] [text item]) r.ingredients)
    in
        div [] 
        [
            h1 [style "font-family" "arial"] 
            [
                a [href recipie.linkToOriginal] [text recipie.title]               
            ], 
            h3 [] [i [] [text "Ingredients:"]], 
            ul [] 
            [
                ingredientsList recipie
            ]
        ] 

        


