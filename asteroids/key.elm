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
            Right

        38 ->
            Up

        39 ->
            Left

        40 ->
            Down

        _ ->
            Unknown
