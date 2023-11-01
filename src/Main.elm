module Main exposing (Model, Msg(..), Triforce(..), Visibility(..), content, init, main, update, visible)

import Browser
import Browser.Events exposing (Visibility(..))
import Element as E
import Element.Input as EI
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Triforce
    = Expected
    | Actual
    | Diff


type alias Contour =
    List String


type Visibility a
    = Visible a
    | Hidden a


visible vh =
    case vh of
        Visible _ ->
            True

        Hidden _ ->
            False


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


type alias Model =
    { expected : Visibility Contour
    , actual : Visibility Contour
    , diff : Visibility Contour
    }


init : Model
init =
    { expected = Visible [ "Expected", "Common" ], actual = Visible [ "Common", "Actual" ], diff = Visible [ "Expected", "Actual" ] }


type Msg
    = Toggle Triforce


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle Expected ->
            { model | expected = toggle model.expected }

        Toggle Actual ->
            { model | actual = toggle model.actual }

        Toggle Diff ->
            { model | diff = toggle model.diff }


text_of_visible : Visibility (List String) -> E.Element msg
text_of_visible v =
    case v of
        Visible sl ->
            E.text (String.join "|" sl)

        Hidden _ ->
            E.text "<...>"


display_triforce : Model -> E.Element Msg
display_triforce model =
    E.column [] [ text_of_visible model.expected, text_of_visible model.actual, text_of_visible model.diff ]


toggle_button : Triforce -> String -> E.Element Msg
toggle_button t label =
    EI.button [] { onPress = Just (Toggle t), label = E.text label }


toggle_buttons : E.Element Msg
toggle_buttons =
    E.column []
        [ toggle_button Expected "Expected"
        , toggle_button Actual "Actual"
        , toggle_button Diff "Diff"
        ]


view : Model -> Html Msg
view model =
    E.layout [] <| E.row [] [ display_triforce model, toggle_buttons ]
