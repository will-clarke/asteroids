module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { myIp : String
    , counter : Int
    }


type Msg
    = FetchMyIP (Result Http.Error String)
    | Click


init : ( Model, Cmd Msg )
init =
    ( Model "N/A" 0, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (toString model)
        , Html.br [] []
        , Html.text (toString model.myIp)
        , Html.br [] []
        , Html.button
            [ Html.Attributes.style [ ( "background-color", "red" ) ]
            , Html.Events.onClick Click
            ]
            [ Html.text "Click me plz" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | counter = model.counter + 1 }, pleaseFetchMyIp )

        FetchMyIP text ->
            case text of
                Err error ->
                    ( { model | myIp = (toString error) }, Cmd.none )

                Ok string ->
                    ( { model | myIp = string }, Cmd.none )


pleaseFetchMyIp : Cmd Msg
pleaseFetchMyIp =
    Http.send FetchMyIP <| Http.getString "https://jsonip.com/"
