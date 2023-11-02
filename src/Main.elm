module Main exposing (Model, Msg(..), Triforce(..), init, main, update)

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
import Visibility as V exposing (content, isVisible, toggle)


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Triforce
    = Expected
    | Actual
    | Diff


type alias Model =
    { expected : V.Visibility Contour
    , actual : V.Visibility Contour
    , diff : V.Visibility Contour
    , image : V.Visibility String
    }


type alias Flags =
    Maybe
        { image_url : String
        }


expected : Contour
expected =
    [ base_square, diff_expected ]


actual : Contour
actual =
    [ base_square, diff_actual ]


diff : Contour
diff =
    [ diff_expected, diff_actual ]


base_square : List Contour.Point
base_square =
    [ point 0 0, point 0 100, point 100 100, point 100 0, point 0 0 ]


diff_expected : List Contour.Point
diff_expected =
    [ point 0 100, point 25 150, point 50 100, point 0 100 ]


diff_actual : List Contour.Point
diff_actual =
    [ point 50 100, point 75 150, point 100 100, point 50 100 ]


init : Flags -> ( Model, Cmd.Cmd Msg )
init flags =
    ( { expected = V.Visible expected, actual = V.Visible actual, diff = V.Visible diff, image = V.Hidden (flags |> Maybe.map (\f -> f.image_url) |> Maybe.withDefault "") }, Cmd.none )


type Msg
    = ToggleContour Triforce
    | ToggleImage
    | ChangeImageUrl String


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    let
        m =
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
                    { model | image = V.Visible s }
    in
    ( m, Cmd.none )


triforce_color : Triforce -> E.Color
triforce_color t =
    case t of
        Expected ->
            E.rgb255 0 255 0

        Actual ->
            E.rgb255 0 0 255

        Diff ->
            E.rgb255 255 0 0


triforce_label : Triforce -> String
triforce_label t =
    case t of
        Expected ->
            "Expected"

        Actual ->
            "Actual"

        Diff ->
            "Diff"


default_border_attributes : List (E.Attribute msg)
default_border_attributes =
    [ EB.width 2, EB.rounded 5 ]


toggle_contour_button : Triforce -> E.Element Msg
toggle_contour_button t =
    EI.button ([ EB.color (triforce_color t) ] ++ default_border_attributes) { onPress = Just (ToggleContour t), label = E.text (triforce_label t) }


toggle_image_button : E.Element Msg
toggle_image_button =
    EI.button ([ EB.color (E.rgb255 0 0 0) ] ++ default_border_attributes) { onPress = Just ToggleImage, label = E.text "Image" }


toggle_buttons : E.Element Msg
toggle_buttons =
    E.column [ E.spacing 10 ]
        [ toggle_contour_button Expected
        , toggle_contour_button Actual
        , toggle_contour_button Diff
        , toggle_image_button
        ]


svg_area : List (Svg.Attribute msg)
svg_area =
    [ SvgAttr.width "500", SvgAttr.height "500", SvgAttr.viewBox "-1 -1 500 500" ]


svg_image_link : { a | image : V.Visibility String } -> Svg.Attribute msg
svg_image_link model =
    SvgAttr.xlinkHref (content model.image)


svg_image : Model -> List (Svg.Svg msg)
svg_image model =
    if isVisible model.image then
        [ Svg.image [ svg_image_link model, SvgAttr.opacity "0.66", SvgAttr.x "0", SvgAttr.y "0", SvgAttr.width "240", SvgAttr.height "159" ] [] ]

    else
        []


svg_path_of_visible : ( V.Visibility Contour, String ) -> List (Svg.Svg msg)
svg_path_of_visible ( v, color ) =
    let
        stroke_visibility =
            if isVisible v then
                SvgAttr.strokeOpacity "1.0"

            else
                SvgAttr.strokeOpacity "0.0"

        base =
            [ SvgAttr.stroke color, SvgAttr.fillOpacity "0.0", stroke_visibility ]

        contour =
            content v
    in
    List.map (\c -> Svg.path (SvgAttr.d (Contour.Svg.d c) :: base) []) contour


svg_contours : Model -> List (Svg.Svg Msg)
svg_contours model =
    List.concatMap svg_path_of_visible [ ( model.expected, "green" ), ( model.actual, "blue" ), ( model.diff, "red" ) ]


svg_window : Model -> E.Element Msg
svg_window model =
    E.el (E.padding 10 :: default_border_attributes) <| E.html <| Svg.svg svg_area (svg_image model ++ svg_contours model)


image_url_field : Model -> E.Element Msg
image_url_field model =
    EI.text default_border_attributes { text = content model.image, onChange = \t -> ChangeImageUrl t, placeholder = Nothing, label = EI.labelRight [] (E.text "Image url") }


view : Model -> Html Msg
view model =
    E.layout [] <| E.row [] [ svg_window model, toggle_buttons, image_url_field model ]
