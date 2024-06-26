module Main exposing (DevelopmentSettings, Flags, ImageFraming, Model, Msg(..), Triforce(..), init, main, update, view)

import Area exposing (Area, ReferentialOrigin(..), ZoomedArea)
import Area.Svg
import Browser
import Components
import Contour exposing (Contour, translate_contour_to_referential)
import Contour.Svg
import Element as E
import Element.Background as EBack
import Element.Border as EB
import Element.Input as EI
import Html exposing (Html)
import Init exposing (ImageSpec, Init)
import Json.Decode as Json
import Qol.Cycle as Cycle exposing (Cycle)
import String exposing (fromFloat)
import Svg
import Svg.Attributes as SvgAttr
import Visibility exposing (Visibility(..), content, isHighlighted, isVisible, toggle)


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.row [ E.spacing 20 ]
            [ svg_window model
            , toggle_buttons model
            , Components.referential_selector [] model.contoursReferential ChangeLayoutReferential
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


extra_colors : Cycle ( number, number, number )
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


{-| Expressed in fraction of Area.maxZoom
-}
zoom_factor : Float
zoom_factor =
    0.05


{-| Expressed in fraction of zoomedArea minimal dimension
-}
drag_factor : Float
drag_factor =
    0.001



-- model


type alias Model =
    { expected : Visibility Contour
    , actual : Visibility Contour
    , diff : Visibility Contour
    , extras : List ( String, Visibility Contour )
    , image : Maybe (Visibility ImageSpec)
    , imageFraming : ImageFraming
    , contoursReferential : ReferentialOrigin
    , zoomedArea : ZoomedArea
    , developmentSettings : DevelopmentSettings
    , mouseDragTracker : Components.MouseDragTracker
    }


type alias ImageFraming =
    { shiftX : Float
    , shiftY : Float
    }


type alias DevelopmentSettings =
    { imageScaling : Int
    , strokeWidth : Float
    , strokeWidthCurrent : String
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
    { shiftX = 0.0, shiftY = 0.0 }


init_dev_settings : Maybe Init -> DevelopmentSettings
init_dev_settings flags =
    flags
        |> Maybe.map dev_settings_from_init
        |> Maybe.withDefault
            { imageScaling = 1, strokeWidth = 0.1, strokeWidthCurrent = "0.1" }


dev_settings_from_init : Init -> DevelopmentSettings
dev_settings_from_init flags =
    let
        ( ( minX, minY ), ( maxX, maxY ) ) =
            extremas flags

        ( dx, dy ) =
            ( maxX - minX, maxY - minY )

        dist =
            sqrt (dx * dx + dy * dy)

        dist_1000 =
            dist / 1000
    in
    { imageScaling = 1, strokeWidth = dist_1000, strokeWidthCurrent = fromFloat dist_1000 }


all_points : Init -> List ( Float, Float )
all_points flags =
    List.concatMap List.concat [ flags.actual, flags.expected, flags.diff ]


extremas : Init -> ( ( Float, Float ), ( Float, Float ) )
extremas flags =
    all_points flags
        |> List.foldl
            (\( x, y ) acc ->
                acc
                    |> Maybe.map
                        (\( ( minX, minY ), ( maxX, maxY ) ) ->
                            ( ( min x minX, min y minY ), ( max x maxX, max y maxY ) )
                        )
                    |> Maybe.withDefault ( ( x, y ), ( x, y ) )
                    |> Just
            )
            Nothing
        |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )


init : Flags -> ( Model, Cmd.Cmd Msg )
init flags =
    let
        decoded =
            flags |> Maybe.andThen (Json.decodeValue Init.decode >> Result.toMaybe)

        image =
            init_image decoded

        expected =
            init_expected decoded

        actual =
            init_actual decoded

        diff =
            init_diff decoded

        extras =
            init_extras decoded

        zoomedArea =
            Area.initZoom (full_area (Maybe.map content image) ([ expected, actual, diff ] ++ List.map Tuple.second extras))
    in
    ( { expected = expected
      , actual = actual
      , diff = diff
      , extras = extras
      , image = image
      , imageFraming = init_image_view
      , contoursReferential = TopLeft
      , developmentSettings = init_dev_settings decoded
      , zoomedArea = zoomedArea
      , mouseDragTracker = Components.initMouseDragTracker
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
    | ZoomBy Float
    | Drag ( Components.MouseDragTracker, Maybe Components.DragMove )


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

                ZoomBy z ->
                    zoomBy z model

                Drag d ->
                    drag d model
    in
    ( m, Cmd.none )


toggled_image : Model -> Maybe (Visibility ImageSpec)
toggled_image model =
    model.image
        |> Maybe.map
            (\i ->
                case i of
                    Hidden img ->
                        Visible img

                    Visible img ->
                        Hidden img

                    Highlighted img ->
                        Hidden img
            )


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
    { model | imageFraming = imageView }



{- **TODO** Development settings are becoming too complex with the "one single update message" approach
   Split update messages to avoid handling too much complexity in this function
-}


changedDevSettings : DevelopmentSettings -> Model -> Model
changedDevSettings ds model =
    let
        scaled =
            if 0 < ds.imageScaling then
                ds

            else
                { ds | imageScaling = model.developmentSettings.imageScaling }

        updated =
            scaled
                |> (\settings -> Maybe.withDefault settings (String.toFloat ds.strokeWidthCurrent |> Maybe.map (\f -> { settings | strokeWidth = f })))
                |> (\settings -> { settings | strokeWidthCurrent = ds.strokeWidthCurrent })
    in
    { model | developmentSettings = updated }


zoomBy : Float -> Model -> Model
zoomBy by model =
    let
        area =
            Area.zoom by model.zoomedArea
    in
    { model | zoomedArea = area }


drag : ( Components.MouseDragTracker, Maybe Components.DragMove ) -> Model -> Model
drag ( tracker, move ) model =
    case move of
        Nothing ->
            { model | mouseDragTracker = tracker }

        Just ( x, y ) ->
            let
                img =
                    model.imageFraming

                step =
                    dragStep model

                dragged =
                    { img | shiftX = img.shiftX - x * step, shiftY = img.shiftY - y * step }
            in
            { model | mouseDragTracker = tracker, imageFraming = dragged }


dragStep : Model -> Float
dragStep model =
    let
        zoomedArea =
            Area.zoomed model.zoomedArea
    in
    min zoomedArea.height zoomedArea.width * drag_factor



-- view


triforce_color : Triforce -> ( Int, Int, Int )
triforce_color t =
    case t of
        Expected ->
            expected_color

        Actual ->
            actual_color

        Diff ->
            diff_color


triforce_label : Triforce -> String
triforce_label t =
    case t of
        Expected ->
            "Expected"

        Actual ->
            "Actual"

        Diff ->
            "Diff"


toggle_visibility_label : Visibility a -> String
toggle_visibility_label visible =
    case visible of
        Visible _ ->
            "(click to hide)"

        Hidden _ ->
            "(click to show)"

        Highlighted _ ->
            "(click to stroke)"


image_controls : Model -> E.Element Msg
image_controls model =
    E.column (bordered [ E.spacing 10, E.width E.fill ]) [ image_shift_x_slider model, image_shift_y_slider model ]


development_settings : Model -> E.Element Msg
development_settings model =
    E.column (bordered [ E.width E.fill, E.spacing 10, E.above (E.el [ E.padding 5 ] (E.text "Development settings")) ])
        [ image_scaling_slider model.developmentSettings
        , stroke_width_field model
        , image_controls model
        ]


image_scaling_slider : DevelopmentSettings -> E.Element Msg
image_scaling_slider devSettings =
    EI.slider [ E.height (E.px 10), E.centerY, sliderTrack ]
        { onChange = \f -> ChangeDevSettings { devSettings | imageScaling = floor f }
        , label = EI.labelRight [ E.alignRight, E.centerY ] (E.text "Image scaling")
        , min = 1.0
        , max = 10.0
        , step = Just 1
        , thumb = EI.defaultThumb
        , value = toFloat devSettings.imageScaling
        }


mouse_wheel_event_listener : Model -> Components.CustomEvent Msg
mouse_wheel_event_listener model =
    let
        step =
            zoomStep model
    in
    Components.mouse_wheel_listener
        (\f ->
            if f < 0 then
                ZoomBy step

            else
                ZoomBy -step
        )


zoomStep : Model -> Float
zoomStep model =
    Area.maxZoom model.zoomedArea * zoom_factor


stroke_width_field : Model -> E.Element Msg
stroke_width_field model =
    let
        current =
            model.developmentSettings
    in
    EI.text []
        { onChange =
            \t -> ChangeDevSettings { current | strokeWidthCurrent = t }
        , text = current.strokeWidthCurrent
        , placeholder = Nothing
        , label = EI.labelRight [ E.alignRight ] (E.text "Stroke width")
        }


image_shift_x_slider : Model -> E.Element Msg
image_shift_x_slider model =
    let
        currentView =
            model.imageFraming

        bound =
            toFloat (imgWidth model)
    in
    EI.slider [ E.height (E.px 10), E.centerY, sliderTrack ]
        { onChange = \f -> ChangeImageView { currentView | shiftX = f }
        , label = EI.labelRight [ E.alignRight, E.centerY ] (E.text "< X >")
        , min = -bound
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
    EI.slider [ E.height (E.px 10), E.centerY, sliderTrack ]
        { onChange = \f -> ChangeImageView { currentView | shiftY = f }
        , label = EI.labelRight [ E.alignRight, E.centerY ] (E.text "^ Y v")
        , min = -bound
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


toggle_contour_button : Visibility Triforce -> E.Element Msg
toggle_contour_button v =
    let
        t =
            content v

        label =
            spaced [ triforce_label t, toggle_visibility_label v ]

        color =
            triforce_color t
    in
    EI.button (bordered [ EB.color (rgb255 color), visibility_button_background v color ]) { onPress = Just (ToggleContour t), label = E.text label }


visibility_button_background : Visibility a -> ( Int, Int, Int ) -> E.Attr decorative Msg
visibility_button_background v ( r, g, b ) =
    case v of
        Visible _ ->
            EBack.color (E.rgba255 r g b 0.1)

        Hidden _ ->
            EBack.color (E.rgba255 25 25 25 0.1)

        Highlighted _ ->
            EBack.color (E.rgba255 r g b 0.7)


toggle_image_button : Visibility img -> E.Element Msg
toggle_image_button v =
    let
        label =
            "Image " ++ toggle_visibility_label v
    in
    EI.button (bordered [ EB.color (E.rgb255 0 0 0) ]) { onPress = Just ToggleImage, label = E.text label }


toggle_buttons : Model -> E.Element Msg
toggle_buttons model =
    E.column (bordered [ E.spacing 10 ])
        ([ toggle_contour_button (model.expected |> Visibility.map (\_ -> Expected))
         , toggle_contour_button (model.actual |> Visibility.map (\_ -> Actual))
         , toggle_contour_button (model.diff |> Visibility.map (\_ -> Diff))
         ]
            ++ toggle_image_buttons model
            ++ toggle_extra_buttons model
        )


toggle_image_buttons : Model -> List (E.Element Msg)
toggle_image_buttons model =
    model.image |> Maybe.map (\img -> [ toggle_image_button img ]) |> Maybe.withDefault []


toggle_extra_button : Int -> ( String, Visibility a ) -> E.Element Msg
toggle_extra_button i ( n, v ) =
    let
        label =
            spaced [ n, toggle_visibility_label v ]

        color =
            Cycle.get i extra_colors
    in
    EI.button (bordered [ EB.color (rgb255 color), visibility_button_background v color ]) { onPress = Just (ToggleExtra i), label = E.text label }


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


area_of_contours : List (Visibility Contour) -> Area
area_of_contours contours =
    contours
        |> List.map content
        |> List.foldl (\c a -> Contour.expand_for_contour a c) Area.min_area


zoomed_area : Model -> Area
zoomed_area model =
    model.zoomedArea
        |> Area.horizontal_shift model.imageFraming.shiftX
        |> Area.vertical_shift model.imageFraming.shiftY
        |> Area.zoomed


full_area : Maybe ImageSpec -> List (Visibility Contour) -> Area
full_area image contours =
    image
        |> Maybe.map area_of_image
        |> Maybe.withDefault (area_of_contours contours)


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
    [ svg_width (toFloat w), svg_height (toFloat h) ]


svg_viewport : Model -> List (Svg.Attribute Msg)
svg_viewport model =
    svg_viewport_pixel_dimensions model ++ [ svg_viewbox model ]


svg_image_link : ImageSpec -> Svg.Attribute msg
svg_image_link img =
    SvgAttr.xlinkHref img.url


svg_width : Float -> Svg.Attribute msg
svg_width w =
    SvgAttr.width (String.fromFloat w)


svg_height : Float -> Svg.Attribute msg
svg_height h =
    SvgAttr.height (String.fromFloat h)


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
                [ Svg.image [ svg_image_link img, SvgAttr.opacity "1.0", svg_x img.refX, svg_y img.refY, svg_width (toFloat img.width * img.pixelWidth), svg_height (toFloat img.height * img.pixelHeight) ] [] ]
            )
        |> Maybe.withDefault []


svg_path_of_visible : Float -> ( Visibility Contour, ( Int, Int, Int ) ) -> List (Svg.Svg msg)
svg_path_of_visible stroke_width ( contour_visibility, color ) =
    let
        stroke_visibility =
            if isVisible contour_visibility then
                SvgAttr.strokeOpacity "1.0"

            else
                SvgAttr.strokeOpacity "0.0"

        fill_visibility =
            if isHighlighted contour_visibility then
                SvgAttr.fillOpacity "0.15"

            else
                SvgAttr.fillOpacity "0.0"

        base =
            [ SvgAttr.stroke (svg255 color), SvgAttr.fill (svg255 color), fill_visibility, SvgAttr.strokeWidth (String.fromFloat stroke_width), stroke_visibility ]

        contour =
            content contour_visibility
    in
    List.map (\c -> Svg.path (SvgAttr.d (Contour.Svg.d c) :: base) []) contour


svg_contours : Model -> List (Svg.Svg Msg)
svg_contours model =
    let
        area =
            Area.full model.zoomedArea

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
            mouse_wheel_event_listener model

        svgListenDrag =
            Components.mouse_drag_listener model.mouseDragTracker (\t m -> Drag ( t, m ))

        svgElements =
            svg_image model ++ svg_contours model

        svgAttributes =
            svg_viewport model ++ (svgListenWheel :: svgListenDrag |> List.map Components.svg_event)

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


spaced : List String -> String
spaced strings =
    String.join " " strings
