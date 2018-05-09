-- module Main exposing (..)


module Main exposing (..)

import Window
import Keyboard
import Html
import Task
import Svg
import Set
import Svg.Attributes
import Key


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Ship =
    { x : Int, y : Int, vx : Int, vy : Int }


type alias WindowSize =
    { x : Int, y : Int }


type alias Model =
    { ship : Ship
    , paused : Bool
    , windowSize : WindowSize
    , keysDown : Set.Set Keyboard.KeyCode
    , log : String
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type Msg
    = Move Direction
    | PauseToggle
    | SetWindowSize Window.Size
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            ( model, Cmd.none )

        PauseToggle ->
            ( { model | paused = not model.paused }, Cmd.none )

        SetWindowSize size ->
            ( { model
                | windowSize = WindowSize size.width (size.height - 50)
                , ship = Ship (size.width // 2) (size.height // 2) model.ship.vx model.ship.vx
              }
            , Cmd.none
            )

        KeyDown keycode ->
            ( keysPressed { model | keysDown = Set.insert keycode model.keysDown }, Cmd.none )

        KeyUp keycode ->
            ( { model | keysDown = Set.remove keycode model.keysDown }, Cmd.none )


keysPressed : Model -> Model
keysPressed model =
    let
        foldlFunction keycode model =
            keyPressed keycode model
    in
        Set.foldl foldlFunction model model.keysDown


keyPressed : Keyboard.KeyCode -> Model -> Model
keyPressed keycode model =
    let
        ship =
            model.ship
    in
        case Key.fromCode keycode of
            Key.Up ->
                { model | ship = Ship ship.x ship.y ship.vx (ship.vy - 10) }

            Key.Down ->
                { model | ship = Ship ship.x ship.y ship.vx (ship.vy + 10) }

            Key.Left ->
                { model | ship = Ship ship.x ship.y (ship.vx - 10) ship.vy }

            Key.Right ->
                { model | ship = Ship ship.x ship.y (ship.vx + 10) ship.vy }

            _ ->
                model


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ship 50 50 0 0)
        False
        (WindowSize 0 0)
        Set.empty
        ""
    , Task.perform SetWindowSize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\winSize -> SetWindowSize winSize)
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text
            ("Keydowns: "
                ++ (toString model.keysDown)
                ++ " --- Ship.x: "
                ++ (toString model.ship.x)
                ++ " --- right button pressed: "
                ++ (toString (Set.member 39 model.keysDown))
            )
        , Html.br [] []
        , Svg.svg
            [ Svg.Attributes.width (toString model.windowSize.x)
            , Svg.Attributes.height (toString model.windowSize.y)
            , Svg.Attributes.viewBox "0 0 (toString model.windowSize.x)(toString model.windowSize.y)"
            ]
            [ Svg.rect
                [ Svg.Attributes.x "25"
                , Svg.Attributes.y "25"
                , Svg.Attributes.width "50"
                , Svg.Attributes.height "50"
                , Svg.Attributes.rx "15"
                , Svg.Attributes.ry "15"
                ]
                []
            , Svg.circle
                [ Svg.Attributes.cx (toString model.ship.x)
                , Svg.Attributes.cy (toString model.ship.y)
                , Svg.Attributes.r "40"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.fill "red"
                ]
                []
            ]
        ]
