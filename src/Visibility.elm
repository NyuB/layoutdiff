module Visibility exposing (Visibility(..), content, isHighlighted, isVisible, map, toggle)

-- exposed


type Visibility a
    = Visible a
    | Highlighted a
    | Hidden a


isVisible : Visibility a -> Bool
isVisible vh =
    case vh of
        Visible _ ->
            True

        Highlighted _ ->
            True

        Hidden _ ->
            False


isHighlighted : Visibility a -> Bool
isHighlighted vh =
    case vh of
        Visible _ ->
            False

        Highlighted _ ->
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

        Highlighted h ->
            h

        Hidden h ->
            h


{-| Switch visibility
-}
toggle : Visibility a -> Visibility a
toggle visibility =
    case visibility of
        Visible v ->
            Hidden v

        Highlighted v ->
            Visible v

        Hidden h ->
            Highlighted h


{-| Apply f to the wrapped content
-}
map : (a -> b) -> Visibility a -> Visibility b
map f visibility =
    case visibility of
        Visible value ->
            Visible (f value)

        Highlighted value ->
            Highlighted (f value)

        Hidden value ->
            Hidden (f value)
