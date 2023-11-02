module Main exposing (Model, Msg(..), Triforce(..), Visibility(..), content, init, main, update, visible)

import Browser
import Browser.Events exposing (Visibility(..))
import Contour exposing (Contour, point)
import Contour.Svg
import Element as E
import Element.Border as EB
import Element.Input as EI
import Html exposing (Html)
import Svg
import Svg.Attributes as SvgAttr


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Triforce
    = Expected
    | Actual
    | Diff


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
    , image : Visibility String
    }


expected =
    [ base_square, diff_expected ]


actual =
    [ base_square, diff_actual ]


diff =
    [ diff_expected, diff_actual ]


base_square =
    [ point 0 0, point 0 100, point 100 100, point 100 0, point 0 0 ]


diff_expected =
    [ point 0 100, point 25 150, point 50 100, point 0 100 ]


diff_actual =
    [ point 50 100, point 75 150, point 100 100, point 50 100 ]


init : Model
init =
    { expected = Visible expected, actual = Visible actual, diff = Visible diff, image = Hidden "" }


type Msg
    = ToggleContour Triforce
    | ToggleImage
    | ChangeImageUrl String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleContour Expected ->
            { model | expected = toggle model.expected }

        ToggleContour Actual ->
            { model | actual = toggle model.actual }

        ToggleContour Diff ->
            { model | diff = toggle model.diff }

        ToggleImage ->
            { model | image = toggle model.image }

        ChangeImageUrl s ->
            { model | image = Visible s }


triforce_color t =
    case t of
        Expected ->
            E.rgb255 0 255 0

        Actual ->
            E.rgb255 0 0 255

        Diff ->
            E.rgb255 255 0 0


triforce_label t =
    case t of
        Expected ->
            "Expected"

        Actual ->
            "Actual"

        Diff ->
            "Diff"


default_border =
    [ EB.width 2, EB.rounded 5 ]


toggle_contour_button : Triforce -> E.Element Msg
toggle_contour_button t =
    EI.button ([ EB.color (triforce_color t) ] ++ default_border) { onPress = Just (ToggleContour t), label = E.text (triforce_label t) }


toggle_image_button =
    EI.button ([ EB.color (E.rgb255 0 0 0) ] ++ default_border) { onPress = Just ToggleImage, label = E.text "Image" }


toggle_buttons : E.Element Msg
toggle_buttons =
    E.column [ E.spacing 10 ]
        [ toggle_contour_button Expected
        , toggle_contour_button Actual
        , toggle_contour_button Diff
        , toggle_image_button
        ]


svg_area =
    [ SvgAttr.width "500", SvgAttr.height "500", SvgAttr.viewBox "-1 -1 500 500" ]


dev_image model =
    SvgAttr.xlinkHref (content model.image)


svg_image model =
    if visible model.image then
        [ Svg.image [ dev_image model, SvgAttr.opacity "0.66", SvgAttr.x "0", SvgAttr.y "0", SvgAttr.width "240", SvgAttr.height "159" ] [] ]

    else
        []


svg_path_of_visible ( v, color ) =
    let
        base =
            [ SvgAttr.stroke color, SvgAttr.fillOpacity "0.0" ]
    in
    case v of
        Visible contour ->
            List.map (\c -> Svg.path ([ SvgAttr.strokeOpacity "1.0", SvgAttr.d (Contour.Svg.d c) ] ++ base) []) contour

        Hidden contour ->
            List.map (\c -> Svg.path ([ SvgAttr.strokeOpacity "0.0", SvgAttr.d (Contour.Svg.d c) ] ++ base) []) contour


svg_contours : Model -> List (Svg.Svg Msg)
svg_contours model =
    List.concatMap svg_path_of_visible [ ( model.expected, "green" ), ( model.actual, "blue" ), ( model.diff, "red" ) ]


svg_window model =
    E.el (E.padding 10 :: default_border) <| E.html <| Svg.svg svg_area (svg_image model ++ svg_contours model)


image_url_field : Model -> E.Element Msg
image_url_field model =
    EI.text default_border { text = content model.image, onChange = \t -> ChangeImageUrl t, placeholder = Nothing, label = EI.labelRight [] (E.text "Image url") }


view : Model -> Html Msg
view model =
    E.layout [] <| E.row [] [ svg_window model, toggle_buttons, image_url_field model ]
