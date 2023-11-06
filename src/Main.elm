module Main exposing (Flags, Model, Msg(..), Triforce(..), init, main, update, view)

import Browser
import Components exposing (..)
import Contour exposing (Contour, ReferentialOrigin(..), point, translate_contour_to_referential)
import Contour.Svg
import Element as E
import Element.Background as EBack
import Element.Border as EB
import Element.Input as EI
import Html exposing (Html)
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
        E.row [ E.spacing 10 ]
            [ svg_window model
            , toggle_buttons model
            , image_controls model
            , referential_selector [] model.contoursReferential ChangeLayoutReferential
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


extra_colors =
    Cycle.ofList ( 255, 255, 0 )
        [ ( 255, 0, 255 )
        , ( 0, 255, 255 )
        , ( 255, 125, 125 )
        , ( 125, 255, 125 )
        , ( 125, 125, 255 )
        ]


rgb255 ( r, g, b ) =
    E.rgb255 r g b


svg255 ( r, g, b ) =
    "rgb(" ++ (String.join "," <| List.map String.fromInt [ r, g, b ]) ++ ")"



-- model


type alias Model =
    { expected : Visibility Contour
    , actual : Visibility Contour
    , diff : Visibility Contour
    , extras : List ( String, Visibility Contour )
    , image : Maybe (Visibility ImageSpec)
    , imageScaling : Int
    , contoursReferential : ReferentialOrigin
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
    model.image |> Maybe.map (\vi -> (content vi).width) |> Maybe.withDefault svg_window_width_px


imgHeight : Model -> Int
imgHeight model =
    model.image |> Maybe.map (\vi -> (content vi).height) |> Maybe.withDefault svg_window_height_px



-- init


type alias Flags =
    Maybe
        { image : Maybe ImageSpec
        , expected : List (List ( Float, Float ))
        , actual : List (List ( Float, Float ))
        , diff : List (List ( Float, Float ))
        , extras : List ( String, List (List ( Float, Float )) )
        }


init_image : Flags -> Maybe (Visibility ImageSpec)
init_image flags =
    flags |> Maybe.andThen (\f -> f.image) |> Maybe.map Visible


init_contour : List (List ( Float, Float )) -> List (List Contour.Point)
init_contour flag_contour =
    List.map (\points -> List.map (\( x, y ) -> point x y) points) flag_contour


init_expected : Flags -> Visibility (List (List Contour.Point))
init_expected flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.expected)) |> Maybe.withDefault (Hidden [ [] ])


init_actual : Flags -> Visibility (List (List Contour.Point))
init_actual flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.actual)) |> Maybe.withDefault (Hidden [ [] ])


init_diff : Flags -> Visibility (List (List Contour.Point))
init_diff flags =
    flags |> Maybe.map (\f -> Visible (init_contour f.diff)) |> Maybe.withDefault (Hidden [ [] ])


init_extras : Flags -> List ( String, Visibility (List (List Contour.Point)) )
init_extras flags =
    flags |> Maybe.map (\f -> List.map (\( n, c ) -> ( n, Hidden (init_contour c) )) f.extras) |> Maybe.withDefault []


init : Flags -> ( Model, Cmd.Cmd Msg )
init flags =
    ( { expected = init_expected flags
      , actual = init_actual flags
      , diff = init_diff flags
      , extras = init_extras flags
      , image = init_image flags
      , imageScaling = 1
      , contoursReferential = TopLeft
      }
    , Cmd.none
    )



-- update


type Msg
    = ToggleContour Triforce
    | ToggleExtra Int
    | ToggleImage
    | ChangeImageScaling Int
    | ChangeLayoutReferential ReferentialOrigin


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

                ChangeImageScaling scaling ->
                    if 0 < scaling then
                        { model | imageScaling = scaling }

                    else
                        model

                ChangeLayoutReferential r ->
                    { model | contoursReferential = r }
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
    E.column [ E.spacing 10, E.width E.fill ] [ image_scaling_slider model ]


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
    EI.slider [ E.height (E.px 10), E.width (E.px 100), sliderTrack ]
        { onChange = \f -> ChangeImageScaling (floor f)
        , label = EI.labelBelow [] (E.text "Image scaling")
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


toggle_extra_button i ( n, v ) =
    let
        label =
            n ++ toggle_show_hide_label (isVisible v)

        color =
            Cycle.get i extra_colors
    in
    EI.button ([ EB.color (rgb255 color) ] ++ default_border_attributes) { onPress = Just (ToggleExtra i), label = E.text label }


toggle_extra_buttons model =
    List.indexedMap (\i e -> toggle_extra_button i e) model.extras



-- SVG


area_of_image : ImageSpec -> Contour.Area
area_of_image img =
    let
        w =
            toFloat img.width * img.pixelWidth

        h =
            toFloat img.height * img.pixelHeight
    in
    { origin = point img.refX img.refY, width = w, height = h }


extra_contours : Model -> List (Visibility Contour)
extra_contours model =
    List.map (\e -> e |> Tuple.second) model.extras


area_of_contours : Model -> Contour.Area
area_of_contours model =
    ([ model.expected, model.actual, model.diff ] ++ extra_contours model)
        |> List.map content
        |> List.foldl (\c a -> Contour.expand_for_contour a c) Contour.min_area


area_of_model model =
    model.image |> Maybe.map (\i -> area_of_image (content i)) |> Maybe.withDefault (area_of_contours model)


svg_viewbox : Model -> Svg.Attribute Msg
svg_viewbox model =
    SvgAttr.viewBox (Contour.Svg.viewBox (area_of_model model))


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


svg_path_of_visible : ( Visibility Contour, ( Int, Int, Int ) ) -> List (Svg.Svg msg)
svg_path_of_visible ( v, color ) =
    let
        stroke_visibility =
            if isVisible v then
                SvgAttr.strokeOpacity "1.0"

            else
                SvgAttr.strokeOpacity "0.0"

        base =
            [ SvgAttr.stroke (svg255 color), SvgAttr.fillOpacity "0.0", stroke_visibility ]

        contour =
            content v
    in
    List.map (\c -> Svg.path (SvgAttr.d (Contour.Svg.d c) :: base) []) contour


svg_contours : Model -> List (Svg.Svg Msg)
svg_contours model =
    let
        area =
            area_of_model model

        translation =
            Visibility.map (translate_contour_to_referential area { contourRef = model.contoursReferential, targetRef = TopLeft })

        extras =
            Cycle.associateList (model.extras |> List.map Tuple.second) extra_colors

        contours =
            [ ( model.expected, expected_color ), ( model.actual, actual_color ), ( model.diff, diff_color ) ] ++ extras
    in
    List.concatMap svg_path_of_visible (contours |> List.map (\( pts, c ) -> ( translation pts, c )))


svg_window : Model -> E.Element Msg
svg_window model =
    E.el ([ E.padding 0, E.width (E.px svg_window_width_px), E.height (E.px svg_window_height_px) ] ++ default_border_attributes) <| E.html <| Svg.svg (svg_area model) (svg_image model ++ svg_contours model)
