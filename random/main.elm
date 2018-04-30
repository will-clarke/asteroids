module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dieFace : Int }


dieSvg : Int -> Svg Msg
dieSvg n =
    svg
        [ width "100"
        , height "100"
        , viewBox "0 0 120 120"
        ]
        (List.append
            [ rect
                [ x "0"
                , y "0"
                , width "100"
                , height "100"
                , rx "15"
                , ry "15"
                , Svg.Attributes.style "fill:rgb(0,0,255)"
                ]
                []
            ]
            (dicePipsSvg n)
        )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text (toString model.dieFace) ]
        , button [ onClick Roll ] [ Html.text "Roll" ]
        , dieSvg model.dieFace
        ]


dicePipsSvg : Int -> List (Svg Msg)
dicePipsSvg n =
    case n of
        1 ->
            [ circle
                [ cx "50"
                , cy "50"
                , r "10"
                ]
                []
            ]

        2 ->
            [ circle
                [ cx "25"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "75"
                , r "10"
                ]
                []
            ]

        3 ->
            [ circle
                [ cx "25"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "50"
                , cy "50"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "75"
                , r "10"
                ]
                []
            ]

        4 ->
            [ circle
                [ cx "25"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "25"
                , cy "75"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "75"
                , r "10"
                ]
                []
            ]

        5 ->
            [ circle
                [ cx "25"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "25"
                , cy "75"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "75"
                , r "10"
                ]
                []
            , circle
                [ cx "50"
                , cy "50"
                , r "10"
                ]
                []
            ]

        6 ->
            [ circle
                [ cx "75"
                , cy "50"
                , r "10"
                ]
                []
            , circle
                [ cx "25"
                , cy "50"
                , r "10"
                ]
                []
            , circle
                [ cx "25"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "25"
                , cy "75"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "25"
                , r "10"
                ]
                []
            , circle
                [ cx "75"
                , cy "75"
                , r "10"
                ]
                []
            ]

        _ ->
            [ Svg.circle [] [] ]


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( Model newFace, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( Model 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none