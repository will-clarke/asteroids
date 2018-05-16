module Key exposing (..)


type Key
    = Space
    | Left
    | Right
    | Up
    | Down
    | Control
    | Meta
    | Q
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        81 ->
            Q

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

        17 ->
            Control

        91 ->
            Meta

        _ ->
            Unknown
