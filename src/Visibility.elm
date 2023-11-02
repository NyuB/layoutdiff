module Visibility exposing (..)


type Visibility a
    = Visible a
    | Hidden a


isVisible : Visibility a -> Bool
isVisible vh =
    case vh of
        Visible _ ->
            True

        Hidden _ ->
            False


content : Visibility a -> a
content vh =
    case vh of
        Visible v ->
            v

        Hidden h ->
            h


toggle : Visibility a -> Visibility a
toggle vh =
    case vh of
        Visible v ->
            Hidden v

        Hidden h ->
            Visible h
