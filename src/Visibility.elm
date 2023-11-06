module Visibility exposing (Visibility(..), content, isVisible, map, toggle)

-- exposed


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


{-| Retrieve the wrapped value, visible or not
-}
content : Visibility a -> a
content visibility =
    case visibility of
        Visible v ->
            v

        Hidden h ->
            h


{-| Switch visibility
-}
toggle : Visibility a -> Visibility a
toggle visibility =
    case visibility of
        Visible v ->
            Hidden v

        Hidden h ->
            Visible h


{-| Apply f to the wrapped content
-}
map : (a -> b) -> Visibility a -> Visibility b
map f visibility =
    case visibility of
        Visible value ->
            Visible (f value)

        Hidden value ->
            Hidden (f value)
