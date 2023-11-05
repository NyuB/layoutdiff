module Main_test exposing (..)

import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes
import Main exposing (Flags, Msg(..), Triforce(..), update, view)
import Test exposing (..)
import Test.Html.Query as HQ
import Test.Html.Selector exposing (..)
import Visibility exposing (..)


suite : Test
suite =
    describe "Main logic"
        [ describe "Visibility" (quick_tests visibility_tests)
        , describe "Html rendering" (quick_tests html_tests)
        ]


type alias Quick_test =
    ( String, () -> Expectation )


quick_test : ( String, () -> Expectation ) -> Test
quick_test ( name, t ) =
    test name t


quick_tests : List Quick_test -> List Test
quick_tests l =
    List.map quick_test l


html_tests : List Quick_test
html_tests =
    [ svg_has_size_of_image
    , svg_has_size_of_contours
    ]


visibility_tests : List Quick_test
visibility_tests =
    [ ( "Visibility at init None"
      , \_ -> Expect.equalLists [ False, False, False, False ] (visibility [ init_none.expected, init_none.actual, init_none.diff ] ++ visibility_presence [ init_none.image ])
      )
    , ( "Visibility at init with flags"
      , \_ -> Expect.equalLists [ True, True, True, True ] (visibility [ init_some.expected, init_some.actual, init_some.diff ] ++ visibility_presence [ init_some.image ])
      )
    , ( "Extra ontours are hidden by default"
      , \_ ->
            let
                with_extra =
                    Just { test_flags | extras = [ ( "test", test_expected_contour ) ] }

                model =
                    Main.init with_extra |> Tuple.first
            in
            Expect.equalLists [ False ] (extras_visibility model)
      )
    , ( "Toggle expected visibility"
      , \_ ->
            let
                ( updated, _ ) =
                    update (ToggleContour Expected) init_some
            in
            Expect.equal updated.expected (Hidden (content init_some.expected))
      )
    , ( "Toggle actual visibility"
      , \_ ->
            let
                ( updated, _ ) =
                    update (ToggleContour Actual) init_some
            in
            Expect.equal updated.actual (Hidden (content init_some.actual))
      )
    , ( "Toggle diff visibility"
      , \_ ->
            let
                ( updated, _ ) =
                    update (ToggleContour Diff) init_some
            in
            Expect.equal updated.diff (Hidden (content init_some.diff))
      )
    , ( "Toggle image visibility"
      , \_ ->
            let
                initial =
                    init_some

                ( updated, _ ) =
                    update ToggleImage initial
            in
            Expect.equal [ True, False ] (visibility_presence [ initial.image, updated.image ])
      )
    , ( "If init flags are passed, components are visible from start"
      , \_ ->
            Expect.equal [ True ] (visibility_presence [ init_some.image ])
      )
    ]


extras_visibility : Main.Model -> List Bool
extras_visibility model =
    model.extras |> List.map Tuple.second |> visibility


single_svg : Html msg -> HQ.Single msg
single_svg v =
    v |> HQ.fromHtml |> HQ.find [ tag "svg" ]


svg_has_size_of_image : Quick_test
svg_has_size_of_image =
    ( "SVG window has same dimensions than image"
    , \_ ->
        let
            v =
                view init_some
        in
        single_svg v |> HQ.has [ html_attribute "viewBox" "0 0 10 20", html_attribute "width" "10", html_attribute "height" "20" ]
    )


svg_has_size_of_contours : Quick_test
svg_has_size_of_contours =
    ( "SVG window has dimensions matching contours if no image"
    , \_ ->
        let
            v =
                view init_some_no_image
        in
        single_svg v |> HQ.has [ html_attribute "viewBox" "0 0 100 150", html_attribute "width" "1000", html_attribute "height" "1000" ]
    )


html_attribute : String -> String -> Selector
html_attribute k v =
    attribute (Html.Attributes.attribute k v)


visibility : List (Visibility a) -> List Bool
visibility l =
    List.map isVisible l


visibility_presence : List (Maybe (Visibility a)) -> List Bool
visibility_presence l =
    List.map (\vo -> vo |> Maybe.map isVisible |> Maybe.withDefault False) l


init_none : Main.Model
init_none =
    Main.init Nothing |> Tuple.first


init_some : Main.Model
init_some =
    Main.init (Just test_flags) |> Tuple.first


type alias FlagContour =
    List (List ( Float, Float ))


test_flags : { image : Maybe { url : String, width : number, height : number, refX : Float, refY : Float, pixelWidth : Float, pixelHeight : Float }, expected : FlagContour, actual : FlagContour, diff : FlagContour, extras : List ( String, FlagContour ) }
test_flags =
    { image = Just { url = "test.jpg", width = 10, height = 20, refX = 0.0, refY = 0.0, pixelWidth = 1.0, pixelHeight = 1.0 }, expected = [ [] ], actual = [ [] ], diff = [ [] ], extras = [] }


init_some_no_image : Main.Model
init_some_no_image =
    Main.init init_some_flags |> Tuple.first


init_some_flags : Flags
init_some_flags =
    Just { image = Nothing, expected = test_expected_contour, actual = test_actual_contour, diff = test_diff_contour, extras = [] }


test_expected_contour : List (List ( Float, Float ))
test_expected_contour =
    [ base_contour, diff_expected ]


test_actual_contour : List (List ( Float, Float ))
test_actual_contour =
    [ base_contour, diff_actual ]


test_diff_contour : List (List ( Float, Float ))
test_diff_contour =
    [ diff_expected, diff_actual ]


base_contour : List ( Float, Float )
base_contour =
    [ ( 0, 0 ), ( 0, 100 ), ( 100, 100 ), ( 100, 0 ), ( 0, 0 ) ]


diff_expected : List ( Float, Float )
diff_expected =
    [ ( 0, 100 ), ( 25, 150 ), ( 50, 100 ), ( 0, 100 ) ]


diff_actual : List ( Float, Float )
diff_actual =
    [ ( 50, 100 ), ( 75, 150 ), ( 100, 100 ), ( 50, 100 ) ]
