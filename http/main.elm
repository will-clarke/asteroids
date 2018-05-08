module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
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
    { topic : String
    , gifUrl : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "waiting.gif", Cmd.none )


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl }, Cmd.none )

        NewGif (Err _) ->
            ( model, Cmd.none )



-- NewGif (Ok newUrl) ->
--     ( model, Cmd.none )
-- NewGif (Err _) ->
--     ( model, Cmd.none )
-- NewGif (Ok newUrl) ->
--     ( { model | gifUrl = newUrl }, Cmd.none )
-- NewGif (Err _) ->
--     (model, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


getRandomGif : String -> Cmd Msg
getRandomGif string =
    Cmd.none



-- import Http
-- import Json.Decode as Decode
-- url = "https://jsonip.com/"
-- getMyIp : Http.Request String
-- getMyIp = Http.getString url
-- type Msg = NewIp (Result Http.Error String)
-- getIp = Http.send NewIp <| Http.getString url
