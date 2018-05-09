module Key exposing (..)


type Key
    = Space
    | Left
    | Right
    | Up
    | Down
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        _ ->
            Unknown
