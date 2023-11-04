module Main exposing (Model, Msg(..), Triforce(..), init, main, update, view)

import Browser
import Browser.Events exposing (Visibility(..))
import Contour exposing (Contour, point)
import Contour.Svg
import Element as E
import Element.Background as EBack
import Element.Border as EB
import Element.Input as EI
import Html exposing (Html)
import String exposing (fromFloat)
import Svg
import Svg.Attributes as SvgAttr
import Visibility as V exposing (content, isVisible, toggle)


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


view : Model -> Html Msg
view model =
    E.layout [] <| E.row [ E.spacing 10 ] [ svg_window model, toggle_buttons model, image_controls model ]



-- main display dimensions


svg_window_width_px : Int
svg_window_width_px =
    1000


svg_window_height_px : Int
svg_window_height_px =
    1000



-- model


type alias Model =
    { expected : V.Visibility Contour
    , actual : V.Visibility Contour
    , diff : V.Visibility Contour
    , image : V.Visibility ImageSpec
    , imageScaling : Int
    }


type alias ImageSpec =
    { url : String
    , width : Int
    , height : Int
    , refX : Float
    , refY : Float
    , pixelWidth : Float
    , pixelHeight : Float
    }


imgWidth : Model -> Int
imgWidth model =
    (content model.image).width


imgHeight : Model -> Int
imgHeight model =
    (content model.image).height



-- init


type alias Flags =
    Maybe
        { image : ImageSpec
        , expected : List (List ( Float, Float ))
        , actual : List (List ( Float, Float ))
        , diff : List (List ( Float, Float ))
        }


init_image : Flags -> V.Visibility ImageSpec
init_image flags =
    flags |> Maybe.map (\f -> V.Visible f.image) |> Maybe.withDefault (V.Hidden { url = "", width = 0, height = 0, refX = 0.0, refY = 0.0, pixelWidth = 1.0, pixelHeight = 1.0 })


init_contour : List (List ( Float, Float )) -> List (List Contour.Point)
init_contour flag_contour =
    List.map (\points -> List.map (\( x, y ) -> point x y) points) flag_contour


init_expected : Flags -> V.Visibility (List (List Contour.Point))
init_expected flags =
    flags |> Maybe.map (\f -> V.Visible (init_contour f.expected)) |> Maybe.withDefault (V.Hidden [ [] ])


init_actual : Flags -> V.Visibility (List (List Contour.Point))
init_actual flags =
    flags |> Maybe.map (\f -> V.Visible (init_contour f.actual)) |> Maybe.withDefault (V.Hidden [ [] ])


init_diff : Flags -> V.Visibility (List (List Contour.Point))
init_diff flags =
    flags |> Maybe.map (\f -> V.Visible (init_contour f.diff)) |> Maybe.withDefault (V.Hidden [ [] ])


init : Flags -> ( Model, Cmd.Cmd Msg )
init flags =
    ( { expected = init_expected flags, actual = init_actual flags, diff = init_diff flags, image = init_image flags, imageScaling = 1 }, Cmd.none )



-- update


type Msg
    = ToggleContour Triforce
    | ToggleImage
    | ChangeImageUrl String
    | ChangeImageScaling Int


type Triforce
    = Expected
    | Actual
    | Diff


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
                    let
                        model_image =
                            content model.image

                        updated_image =
                            V.Visible { model_image | url = s }
                    in
                    { model | image = updated_image }

                ChangeImageScaling scaling ->
                    if 0 < scaling then
                        { model | imageScaling = scaling }

                    else
                        model
    in
    ( m, Cmd.none )



-- view


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


toggle_show_hide_label : Bool -> String
toggle_show_hide_label visible =
    if visible then
        "(hide)"

    else
        "(show)"


image_controls : Model -> E.Element Msg
image_controls model =
    E.column [ E.spacing 10, E.width E.fill ] [ image_url_field model, image_scaling_slider model ]


image_url_field : Model -> E.Element Msg
image_url_field model =
    EI.text default_border_attributes { text = (content model.image).url, onChange = \t -> ChangeImageUrl t, placeholder = Nothing, label = EI.labelRight [] (E.text "Image url") }


image_scaling_slider : Model -> E.Element Msg
image_scaling_slider model =
    let
        sliderTrack =
            E.behindContent
                (E.el
                    [ E.width E.fill
                    , E.height (E.px 2)
                    , E.centerY
                    , EBack.color (E.rgb255 0 0 0)
                    , EB.rounded 2
                    ]
                    E.none
                )
    in
    EI.slider [ E.height (E.px 10), E.width E.fill, sliderTrack ]
        { onChange = \f -> ChangeImageScaling (floor f)
        , label = EI.labelHidden "Image scaling"
        , min = 1.0
        , max = 10.0
        , step = Just 1
        , thumb = EI.defaultThumb
        , value = toFloat model.imageScaling
        }


default_border_attributes : List (E.Attribute msg)
default_border_attributes =
    [ EB.width 2, EB.rounded 5 ]


toggle_contour_button : Triforce -> Bool -> E.Element Msg
toggle_contour_button t v =
    let
        label =
            triforce_label t ++ " " ++ toggle_show_hide_label v
    in
    EI.button ([ EB.color (triforce_color t) ] ++ default_border_attributes) { onPress = Just (ToggleContour t), label = E.text label }


toggle_image_button : Bool -> E.Element Msg
toggle_image_button v =
    let
        label =
            "Image " ++ toggle_show_hide_label v
    in
    EI.button ([ EB.color (E.rgb255 0 0 0) ] ++ default_border_attributes) { onPress = Just ToggleImage, label = E.text label }


toggle_buttons : Model -> E.Element Msg
toggle_buttons model =
    E.column [ E.spacing 10 ]
        [ toggle_contour_button Expected (V.isVisible model.expected)
        , toggle_contour_button Actual (V.isVisible model.actual)
        , toggle_contour_button Diff (V.isVisible model.diff)
        , toggle_image_button (V.isVisible model.image)
        ]



-- SVG


svg_viewbox : Model -> Svg.Attribute Msg
svg_viewbox model =
    let
        img =
            content model.image
    in
    SvgAttr.viewBox (Contour.Svg.viewBox { width = img.width, height = img.height, refX = img.refX, refY = img.refY, xUnit = img.pixelWidth, yUnit = img.pixelHeight })


svg_area_dim : Model -> List (Svg.Attribute msg)
svg_area_dim model =
    let
        w =
            min svg_window_width_px (imgWidth model * model.imageScaling)

        h =
            min svg_window_height_px (imgHeight model * model.imageScaling)
    in
    [ svg_width w, svg_height h ]


svg_area : Model -> List (Svg.Attribute Msg)
svg_area model =
    svg_area_dim model ++ [ svg_viewbox model ]


svg_image_link : Model -> Svg.Attribute msg
svg_image_link model =
    SvgAttr.xlinkHref (content model.image).url


svg_width : Int -> Svg.Attribute msg
svg_width w =
    SvgAttr.width (String.fromInt w)


svg_height : Int -> Svg.Attribute msg
svg_height h =
    SvgAttr.height (String.fromInt h)


svg_x : Float -> Svg.Attribute msg
svg_x x =
    SvgAttr.x (fromFloat x)


svg_y : Float -> Svg.Attribute msg
svg_y y =
    SvgAttr.y (fromFloat y)


svg_image : Model -> List (Svg.Svg msg)
svg_image model =
    if isVisible model.image then
        let
            img =
                content model.image
        in
        [ Svg.image [ svg_image_link model, SvgAttr.opacity "1.0", svg_x img.refX, svg_y img.refY, svg_width img.width, svg_height img.height ] [] ]

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
    E.el ([ E.padding 0, E.width (E.px svg_window_width_px), E.height (E.px svg_window_height_px) ] ++ default_border_attributes) <| E.html <| Svg.svg (svg_area model) (svg_image model ++ svg_contours model)
