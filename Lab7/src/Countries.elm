module Countries exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode as Dec
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Attributes exposing (value)
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Country =
    { name : String
    , area : Float
    , region : String
    , population: Int
    }




decodeCountry : Dec.Decoder Country
decodeCountry =
    Dec.map4 Country 
        (Dec.at  ["name", "common"] Dec.string)
        (Dec.field "area" Dec.float)
        (Dec.field "region" Dec.string)
        (Dec.field "population" Dec.int)



type Model
    = Initial
    | RequestSent
    | Success (List Country) Bool String String
    | Error Http.Error



init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial
    , Cmd.none
    )




type Msg
    = GetCountries
    | GotCountries (Result Http.Error (List Country))
    | ChangeSortingOrder Bool
    | ChangeNameFilter String
    | ChangeSortField String



getCountries : Cmd Msg
getCountries = Http.get 
    { url = "https://restcountries.com/v3.1/all"
    , expect = Http.expectJson GotCountries (Dec.list decodeCountry) 
    }



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCountries ->
            ( RequestSent
            , getCountries
            )

        GotCountries (Ok countries) ->
            ( Success countries False "" "population"
            , Cmd.none
            )

        GotCountries (Err err) ->
            ( Error err
            , Cmd.none
            )
        ChangeSortingOrder descending ->
            case model of 
                Success countries _ nameFilter sortField-> (Success countries descending nameFilter sortField, Cmd.none)
                _ -> (model, Cmd.none)
        ChangeNameFilter nameFilter ->
            case model of
                Success countries descending _ sortField-> (Success countries descending nameFilter sortField, Cmd.none)
                _ -> (model, Cmd.none)
        ChangeSortField sortField ->
            case model of 
                Success countries descending nameFilter _ -> (Success countries descending nameFilter sortField, Cmd.none)
                _ -> (model, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




view : Model -> Html Msg
view model =
    case model of
        Initial ->
            viewInitial

        RequestSent ->
            div [] [ text "Loading..." ]

        Success countries descending nameFilter sortField->
            viewSuccess countries descending nameFilter sortField

        Error err ->
            viewError err

viewInitial : Html Msg
viewInitial =
    div []
        [ button [ onClick GetCountries ] [ text "Get countries" ]
        ]


viewCountry : Country -> Html msg
viewCountry {name, area, region, population} =
    div [style "border" "solid 1px", style "margin" "2px"] 
        [ p [] [text <| "Name:" ++ name]
        , p [] [text <| "Area: " ++ String.fromFloat area]
        , p [] [text <| "Population Density: " ++ String.fromFloat((toFloat population) / area)]
        ]

viewSuccess : List Country -> Bool -> String -> String -> Html Msg
viewSuccess countries descending nameFilter filterField =
        div [] ( select [onInput ChangeSortField] [
                    option [value "population"] [text "Population"],
                    option [value "area"] [text "Area"],
                    option [value "population_density"] [text "Populaiton Density"]
                ]
              :: input [type_ "text", value nameFilter, onInput ChangeNameFilter] []
              :: text "Sort Descending"
              :: input [type_ "checkbox", onCheck ChangeSortingOrder, checked descending] [] 
              :: h2 [] [ text "ok" ] 
              :: (List.map viewCountry <|
                    let
                        fieldExtractor : Country -> Country -> (Float, Float)
                        fieldExtractor c1 c2 =
                            case filterField of
                                "population" -> (toFloat c1.population, toFloat c2.population)
                                "area" -> (c1.area, c2.area)
                                "population_density" -> ((toFloat c1.population) / c1.area, (toFloat c2.population) / c2.area)
                                _ -> (toFloat c1.population, toFloat c2.population)
                        comparator =
                            if descending then
                                (\(c1, c2 )-> compare c2 c1)
                            else
                                (\(c1, c2) -> compare c1 c2)
                    in
                        countries 
                        |> List.filter (\{name} -> String.contains nameFilter name) 
                        |> List.sortWith  (\c1 c2 -> comparator (fieldExtractor c1 c2))
                )
                )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "Bad Url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "BadS tatus: " ++ String.fromInt status

        Http.BadBody _ ->
            "Bad Body"


viewError : Http.Error -> Html msg
viewError err =
    div [] [ h2 [] [ text "Rip" ], p [] [ text <| httpErrorToString err ] ]


    
