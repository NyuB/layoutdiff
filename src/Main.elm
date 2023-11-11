module Main exposing (Flags, Model, Msg(..), Triforce(..), init, main, update, view)

import Area exposing (Area, ReferentialOrigin(..))
import Area.Svg
import Browser
import Components exposing (..)
import Contour exposing (Contour, translate_contour_to_referential)
import Contour.Svg
import Element as E
import Element.Background as EBack
import Element.Border as EB
import Element.Input as EI
import Html exposing (Html)
import Init exposing (ImageSpec, Init)
import Json.Decode as Json
import Qol.Cycle as Cycle
import String exposing (fromFloat)
import Svg
import Svg.Attributes as SvgAttr
import Visibility exposing (Visibility(..), content, isVisible, toggle)


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.row [ E.spacing 20 ]
            [ svg_window model
            , toggle_buttons model
            , image_controls model
            , referential_selector [] model.contoursReferential ChangeLayoutReferential
            , development_settings model
            ]



-- main display constants


svg_window_width_px : Int
svg_window_width_px =
    1000


svg_window_height_px : Int
svg_window_height_px =
    1000


expected_color : ( number, number, number )
expected_color =
    ( 0, 255, 0 )


actual_color : ( number, number, number )
actual_color =
    ( 0, 0, 255 )


diff_color : ( number, number, number )
diff_color =
    ( 255, 0, 0 )


extra_colors : Cycle.Cycle ( number, number, number )
extra_colors =
    Cycle.ofList ( 255, 255, 0 )
        [ ( 255, 0, 255 )
        , ( 0, 255, 255 )
        , ( 255, 125, 125 )
        , ( 125, 255, 125 )
        , ( 125, 125, 255 )
        ]


rgb255 : ( Int, Int, Int ) -> E.Color
rgb255 ( r, g, b ) =
    E.rgb255 r g b


svg255 : ( Int, Int, Int ) -> String
svg255 ( r, g, b ) =
    "rgb(" ++ (String.join "," <| List.map String.fromInt [ r, g, b ]) ++ ")"



-- model


type alias Model =
    { expected : Visibility Contour
    , actual : Visibility Contour
    , diff : Visibility Contour
    , extras : List ( String, Visibility Contour )
    , image : Maybe (Visibility ImageSpec)
    , imageFraming : ImageFraming
    , contoursReferential : ReferentialOrigin
    , developmentSettings : DevelopmentSettings
    }


type alias ImageFraming =
    { shiftX : Float
    , shiftY : Float
    , zoom : Float
    }


type alias DevelopmentSettings =
    { imageScaling : Int
    , strokeWidth : Float
    , strokeWidthCurrent : String
    , zoomStep : Float
    }


imgWidth : Model -> Int
imgWidth model =
    model.image |> Maybe.map (\vi -> (content vi).width) |> Maybe.withDefault svg_window_width_px


imgHeight : Model -> Int
imgHeight model =
    model.image |> Maybe.map (\vi -> (content vi).height) |> Maybe.withDefault svg_window_height_px



-- init


type alias Flags =
    Maybe Json.Value


init_image : Maybe Init -> Maybe (Visibility ImageSpec)
init_image i =
    i |> Maybe.andThen (\f -> f.image) |> Maybe.map Visible


init_contour : List (List ( Float, Float )) -> Contour
init_contour flag_contour =
    List.map (\points -> List.map (\( x, y ) -> Area.point x y) points) flag_contour


init_expected : Maybe Init -> Visibility Contour
init_expected flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.expected)) |> Maybe.withDefault (Hidden [ [] ])


init_actual : Maybe Init -> Visibility Contour
init_actual flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.actual)) |> Maybe.withDefault (Hidden [ [] ])


init_diff : Maybe Init -> Visibility Contour
init_diff flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.diff)) |> Maybe.withDefault (Hidden [ [] ])


init_extras : Maybe Init -> List ( String, Visibility Contour )
init_extras flags =
    flags |> Maybe.map (\f -> List.map (\( n, c ) -> ( n, Hidden (init_contour c) )) f.extras) |> Maybe.withDefault []


init_image_view : ImageFraming
init_image_view =
    { zoom = 0.0, shiftX = 0.0, shiftY = 0.0 }


initDevSettings : DevelopmentSettings
initDevSettings =
    { imageScaling = 1, strokeWidth = 0.5, strokeWidthCurrent = "0.5", zoomStep = 0.5 }


init : Flags -> ( Model, Cmd.Cmd Msg )
init flags =
    let
        decoded =
            flags |> Maybe.andThen (Json.decodeValue Init.decode >> Result.toMaybe)
    in
    ( { expected = init_expected decoded
      , actual = init_actual decoded
      , diff = init_diff decoded
      , extras = init_extras decoded
      , image = init_image decoded
      , imageFraming = init_image_view
      , contoursReferential = TopLeft
      , developmentSettings = initDevSettings
      }
    , Cmd.none
    )



-- update


type Msg
    = ToggleContour Triforce
    | ToggleExtra Int
    | ToggleImage
    | ChangeImageView ImageFraming
    | ChangeLayoutReferential ReferentialOrigin
    | ChangeDevSettings DevelopmentSettings


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

                ToggleExtra index ->
                    toggled_extra model index

                ToggleContour Actual ->
                    { model | actual = toggle model.actual }

                ToggleContour Diff ->
                    { model | diff = toggle model.diff }

                ToggleImage ->
                    { model | image = toggled_image model }

                ChangeImageView imageView ->
                    changedImageView imageView model

                ChangeLayoutReferential r ->
                    { model | contoursReferential = r }

                ChangeDevSettings ds ->
                    changedDevSettings ds model
    in
    ( m, Cmd.none )


toggled_image : Model -> Maybe (Visibility ImageSpec)
toggled_image model =
    model.image |> Maybe.map (\i -> toggle i)


toggled_extra : Model -> Int -> Model
toggled_extra model index =
    let
        updated =
            List.indexedMap
                (\i ( n, c ) ->
                    if i == index then
                        ( n, toggle c )

                    else
                        ( n, c )
                )
                model.extras
    in
    { model | extras = updated }


changedImageView : ImageFraming -> Model -> Model
changedImageView imageView model =
    if 0.0 <= imageView.zoom then
        { model | imageFraming = imageView }

    else
        model


changedDevSettings : DevelopmentSettings -> Model -> Model
changedDevSettings ds model =
    let
        scaled =
            if 0 < ds.imageScaling then
                ds

            else
                model.developmentSettings

        stroked =
            String.toFloat ds.strokeWidthCurrent |> Maybe.map (\f -> { scaled | strokeWidth = f }) |> Maybe.withDefault scaled

        withCurrent =
            { stroked | strokeWidthCurrent = ds.strokeWidthCurrent }
    in
    { model | developmentSettings = withCurrent }



-- view


triforce_color : Triforce -> E.Color
triforce_color t =
    case t of
        Expected ->
            rgb255 expected_color

        Actual ->
            rgb255 actual_color

        Diff ->
            rgb255 diff_color


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
    E.column (bordered [ E.spacing 10 ]) [ image_zoom_slider model, image_shift_x_slider model, image_shift_y_slider model ]


development_settings : Model -> E.Element Msg
development_settings model =
    E.column (bordered [ E.width E.fill, E.spacing 10 ]) [ image_scaling_slider model.developmentSettings, stroke_width_field model, wheel_display model.imageFraming, zoom_step_field model.developmentSettings ]


image_scaling_slider : DevelopmentSettings -> E.Element Msg
image_scaling_slider devSettings =
    EI.slider [ E.height (E.px 10), E.width (E.px 100), sliderTrack ]
        { onChange = \f -> ChangeDevSettings { devSettings | imageScaling = floor f }
        , label = EI.labelBelow [] (E.text "Image scaling")
        , min = 1.0
        , max = 10.0
        , step = Just 1
        , thumb = EI.defaultThumb
        , value = toFloat devSettings.imageScaling
        }


wheel_display : ImageFraming -> E.Element Msg
wheel_display imageFraming =
    E.row
        [ E.width E.fill
        , E.spacing 5
        ]
        [ E.el (bordered [ E.width (E.fillPortion 3) ]) (E.text "Zoom")
        , E.el (bordered [ E.width (E.fillPortion 1) ]) (E.text (String.fromFloat imageFraming.zoom |> String.slice 0 10))
        ]


mouse_wheel_event_listener : Model -> Components.CustomEvent Msg
mouse_wheel_event_listener model =
    Components.mouse_wheel_listener (\f -> ChangeImageView (updateZoom f model))


updateZoom : Float -> Model -> ImageFraming
updateZoom z model =
    let
        imageFraming =
            model.imageFraming

        step =
            model.developmentSettings.zoomStep
    in
    if z == 0 then
        imageFraming

    else if z < 0 then
        { imageFraming | zoom = imageFraming.zoom + step }

    else
        { imageFraming | zoom = imageFraming.zoom - step }


zoom_step_field : DevelopmentSettings -> E.Element Msg
zoom_step_field devSettings =
    EI.text []
        { label = EI.labelBelow [] (E.text "Zoom step")
        , placeholder = Nothing
        , text = String.fromFloat devSettings.zoomStep
        , onChange =
            \s ->
                String.toFloat s
                    |> Maybe.map (\f -> ChangeDevSettings { devSettings | zoomStep = f })
                    |> Maybe.withDefault (ChangeDevSettings devSettings)
        }


stroke_width_field : Model -> E.Element Msg
stroke_width_field model =
    let
        current =
            model.developmentSettings
    in
    EI.text [ E.width E.fill, E.height E.fill ]
        { onChange =
            \t -> ChangeDevSettings { current | strokeWidthCurrent = t }
        , text = current.strokeWidthCurrent
        , placeholder = Nothing
        , label = EI.labelBelow [] (E.text "Stroke width")
        }


image_zoom_slider : Model -> E.Element Msg
image_zoom_slider model =
    let
        currentView =
            model.imageFraming
    in
    EI.slider [ E.height (E.px 10), E.width (E.px 100), sliderTrack ]
        { onChange = \f -> ChangeImageView { currentView | zoom = f }
        , label = EI.labelBelow [] (E.text "Zoom")
        , min = 0.0
        , max = toFloat (min (imgHeight model) (imgWidth model)) / 2.0
        , step = Just 1
        , thumb = EI.defaultThumb
        , value = model.imageFraming.zoom
        }


image_shift_x_slider : Model -> E.Element Msg
image_shift_x_slider model =
    let
        currentView =
            model.imageFraming

        bound =
            toFloat (imgWidth model)
    in
    EI.slider [ E.height (E.px 10), E.width (E.px 100), sliderTrack ]
        { onChange = \f -> ChangeImageView { currentView | shiftX = f }
        , label = EI.labelBelow [] (E.text "< X >")
        , min = 0 - bound
        , max = bound
        , step = Just 1.0
        , thumb = EI.defaultThumb
        , value = model.imageFraming.shiftX
        }


image_shift_y_slider : Model -> E.Element Msg
image_shift_y_slider model =
    let
        currentView =
            model.imageFraming

        bound =
            toFloat (imgHeight model)
    in
    EI.slider [ E.height (E.px 10), E.width (E.px 100), sliderTrack ]
        { onChange = \f -> ChangeImageView { currentView | shiftY = f }
        , label = EI.labelBelow [] (E.text "v Y ^")
        , min = 0 - bound
        , max = bound
        , step = Just 1.0
        , thumb = EI.defaultThumb
        , value = model.imageFraming.shiftY
        }


sliderTrack : E.Attribute msg
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


toggle_contour_button : Triforce -> Bool -> E.Element Msg
toggle_contour_button t v =
    let
        label =
            triforce_label t ++ " " ++ toggle_show_hide_label v
    in
    EI.button (bordered [ EB.color (triforce_color t) ]) { onPress = Just (ToggleContour t), label = E.text label }


toggle_image_button : Bool -> E.Element Msg
toggle_image_button v =
    let
        label =
            "Image " ++ toggle_show_hide_label v
    in
    EI.button (bordered [ EB.color (E.rgb255 0 0 0) ]) { onPress = Just ToggleImage, label = E.text label }


toggle_buttons : Model -> E.Element Msg
toggle_buttons model =
    E.column (bordered [ E.spacing 10 ])
        ([ toggle_contour_button Expected (isVisible model.expected)
         , toggle_contour_button Actual (isVisible model.actual)
         , toggle_contour_button Diff (isVisible model.diff)
         ]
            ++ toggle_image_buttons model
            ++ toggle_extra_buttons model
        )


toggle_image_buttons : Model -> List (E.Element Msg)
toggle_image_buttons model =
    model.image |> Maybe.map (\img -> [ toggle_image_button (isVisible img) ]) |> Maybe.withDefault []


toggle_extra_button : Int -> ( String, Visibility a ) -> E.Element Msg
toggle_extra_button i ( n, v ) =
    let
        label =
            n ++ toggle_show_hide_label (isVisible v)

        color =
            Cycle.get i extra_colors
    in
    EI.button (bordered [ EB.color (rgb255 color) ]) { onPress = Just (ToggleExtra i), label = E.text label }


toggle_extra_buttons : Model -> List (E.Element Msg)
toggle_extra_buttons model =
    List.indexedMap (\i e -> toggle_extra_button i e) model.extras



-- SVG


area_of_image : ImageSpec -> Area
area_of_image img =
    let
        w =
            toFloat img.width * img.pixelWidth

        h =
            toFloat img.height * img.pixelHeight
    in
    { origin = Area.point img.refX img.refY, width = w, height = h }


extra_contours : Model -> List (Visibility Contour)
extra_contours model =
    List.map (\e -> e |> Tuple.second) model.extras


area_of_contours : Model -> Area
area_of_contours model =
    ([ model.expected, model.actual, model.diff ] ++ extra_contours model)
        |> List.map content
        |> List.foldl (\c a -> Contour.expand_for_contour a c) Area.min_area


zoomed_area : Model -> Area
zoomed_area model =
    full_area model
        |> Area.shrink_by model.imageFraming.zoom
        |> Area.shift_by_horizontal model.imageFraming.shiftX
        |> Area.shift_by_vertical model.imageFraming.shiftY


full_area : Model -> Area
full_area model =
    model.image
        |> Maybe.map (\i -> area_of_image (content i))
        |> Maybe.withDefault (area_of_contours model)


svg_viewbox : Model -> Svg.Attribute Msg
svg_viewbox model =
    SvgAttr.viewBox (Area.Svg.viewBox (zoomed_area model))


svg_viewport_pixel_dimensions : Model -> List (Svg.Attribute msg)
svg_viewport_pixel_dimensions model =
    let
        w =
            min svg_window_width_px (imgWidth model * model.developmentSettings.imageScaling)

        h =
            min svg_window_height_px (imgHeight model * model.developmentSettings.imageScaling)
    in
    [ svg_width w, svg_height h ]


svg_viewport : Model -> List (Svg.Attribute Msg)
svg_viewport model =
    svg_viewport_pixel_dimensions model ++ [ svg_viewbox model ]


svg_image_link : ImageSpec -> Svg.Attribute msg
svg_image_link img =
    SvgAttr.xlinkHref img.url


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


maybe_if : (a -> Bool) -> Maybe a -> Maybe a
maybe_if predicate opt =
    Maybe.andThen
        (\x ->
            if predicate x then
                Just x

            else
                Nothing
        )
        opt


svg_image : Model -> List (Svg.Svg msg)
svg_image model =
    model.image
        |> maybe_if isVisible
        |> Maybe.map content
        |> Maybe.map
            (\img ->
                [ Svg.image [ svg_image_link img, SvgAttr.opacity "1.0", svg_x img.refX, svg_y img.refY, svg_width img.width, svg_height img.height ] [] ]
            )
        |> Maybe.withDefault []


svg_path_of_visible : Float -> ( Visibility Contour, ( Int, Int, Int ) ) -> List (Svg.Svg msg)
svg_path_of_visible sw ( v, color ) =
    let
        stroke_visibility =
            if isVisible v then
                SvgAttr.strokeOpacity "1.0"

            else
                SvgAttr.strokeOpacity "0.0"

        base =
            [ SvgAttr.stroke (svg255 color), SvgAttr.fillOpacity "0.0", SvgAttr.strokeWidth (String.fromFloat sw), stroke_visibility ]

        contour =
            content v
    in
    List.map (\c -> Svg.path (SvgAttr.d (Contour.Svg.d c) :: base) []) contour


svg_contours : Model -> List (Svg.Svg Msg)
svg_contours model =
    let
        area =
            full_area model

        translation =
            Visibility.map (translate_contour_to_referential area { contourRef = model.contoursReferential, targetRef = TopLeft })

        extras =
            Cycle.associateList (model.extras |> List.map Tuple.second) extra_colors

        contours =
            [ ( model.expected, expected_color ), ( model.actual, actual_color ), ( model.diff, diff_color ) ] ++ extras
    in
    List.concatMap (svg_path_of_visible model.developmentSettings.strokeWidth) (contours |> List.map (\( pts, c ) -> ( translation pts, c )))


svg_window : Model -> E.Element Msg
svg_window model =
    let
        svgListenWheel =
            mouse_wheel_event_listener model |> Components.svg_event

        svgElements =
            svg_image model ++ svg_contours model

        svgAttributes =
            svg_viewport model ++ [ svgListenWheel ]

        svgContent =
            E.html (Svg.svg svgAttributes svgElements)
    in
    E.el (bordered [ E.padding 10, E.width (E.px svg_window_width_px), E.height (E.px svg_window_height_px) ]) svgContent



-- Style helpers


bordered : List (E.Attribute msg) -> List (E.Attribute msg)
bordered attrs =
    default_border_attributes ++ attrs


default_border_attributes : List (E.Attribute msg)
default_border_attributes =
    [ EB.width 2, EB.rounded 5, E.padding 5 ]
