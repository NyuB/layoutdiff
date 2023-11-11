module Main_test exposing (suite)

import Area exposing (ReferentialOrigin(..), point)
import Expect exposing (Expectation)
import Html.Attributes
import Init
import Json.Encode
import Main exposing (Msg(..), Triforce(..), update, view)
import Test exposing (Test, describe, test)
import Test.Html.Query as HQ
import Test.Html.Selector exposing (Selector, attribute, tag)
import Visibility exposing (Visibility(..), content, isVisible)



-- test suites


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
    , svg_path_per_contour
    , svg_path_with_extras
    , svg_path_referential_change
    , svg_path_zoom
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
                    just_flags { test_flags | extras = [ ( "test", test_expected_contour ) ] }

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


svg_has_size_of_image : Quick_test
svg_has_size_of_image =
    ( "SVG window has same dimensions than image"
    , \_ ->
        let
            v =
                view init_some |> HQ.fromHtml
        in
        Expect.all
            [ single_svg >> svg_viewBox_is "0 0 10 20"
            , single_svg >> svg_width_height_are 10 20
            ]
            v
    )


svg_has_size_of_contours : Quick_test
svg_has_size_of_contours =
    ( "SVG window has dimensions matching contours if no image"
    , \_ ->
        let
            v =
                view init_some_no_image |> HQ.fromHtml
        in
        Expect.all
            [ single_svg >> svg_viewBox_is "0 0 100 150"
            , single_svg >> svg_width_height_are 1000 1000
            ]
            v
    )


svg_path_per_contour : Quick_test
svg_path_per_contour =
    ( "Each list of point in each contour has one svg path"
    , \_ ->
        let
            v =
                view init_some_no_image |> HQ.fromHtml

            expected =
                6

            -- 2 in expected, 2 in actual, 2 in diff
        in
        v |> HQ.findAll [ tag "path" ] |> HQ.count (\n -> Expect.equal expected n)
    )


svg_path_with_extras : Quick_test
svg_path_with_extras =
    ( "Extras have their svg path too"
    , \_ ->
        let
            with_extra =
                just_flags { test_flags | extras = [ ( "test", test_expected_contour ) ] }

            model =
                Main.init with_extra |> Tuple.first

            v =
                view model |> HQ.fromHtml

            expected =
                5

            -- 1 in expected, 1 in actual, 1 in diff, 2 in one extra
        in
        v |> HQ.findAll [ tag "path" ] |> HQ.count (\n -> Expect.equal expected n)
    )


svg_path_referential_change : Quick_test
svg_path_referential_change =
    ( "When changing referential, path are converted to adjust to viewBox coordinates"
    , \_ ->
        let
            two_points_in_diff_only =
                init_two_points_in_diff ( 0, 0 ) ( 10, 10 )

            updated =
                update (ChangeLayoutReferential BottomLeft) two_points_in_diff_only |> Tuple.first

            updated_view =
                updated |> view |> HQ.fromHtml
        in
        Expect.all
            [ svg_viewBox_is "0 0 10 10"
            , HQ.find visible_svg_path >> svg_path_is "M 0 10 L 10 0z"
            ]
            updated_view
    )


svg_path_zoom : Quick_test
svg_path_zoom =
    ( "When zooming, viewBox is adjusted but path coordinates stay the same"
    , \_ ->
        let
            two_points_in_diff_only =
                init_two_points_in_diff ( 0, 0 ) ( 10, 10 )

            updated =
                two_points_in_diff_only
                    |> zoom 4.0
                    |> Tuple.first

            updated_view =
                updated |> view |> HQ.fromHtml
        in
        Expect.all
            [ single_svg >> svg_viewBox_is "4 4 2 2"
            , HQ.find visible_svg_path >> svg_path_is "M 0 0 L 10 10z"
            ]
            updated_view
    )



-- helpers


zoom : Float -> Main.Model -> ( Main.Model, Cmd Msg )
zoom z m =
    let
        iv =
            m.imageFraming
    in
    update (ChangeImageView { iv | zoom = z }) m


extras_visibility : Main.Model -> List Bool
extras_visibility model =
    model.extras |> List.map Tuple.second |> visibility


single_svg : HQ.Single msg -> HQ.Single msg
single_svg v =
    HQ.find [ tag "svg" ] v


visible_svg_path : List Selector
visible_svg_path =
    [ tag "path", html_attribute "stroke-opacity" "1.0" ]


svg_viewBox_is : String -> HQ.Single msg -> Expectation
svg_viewBox_is vb query =
    query |> HQ.has [ html_attribute "viewBox" vb ]


svg_path_is : String -> HQ.Single msg -> Expectation
svg_path_is d query =
    query |> HQ.has [ html_attribute "d" d ]


svg_width_height_are : Int -> Int -> HQ.Single msg -> Expectation
svg_width_height_are w h query =
    HQ.has [ html_attribute "width" (String.fromInt w), html_attribute "height" (String.fromInt h) ] query


html_attribute : String -> String -> Selector
html_attribute k v =
    attribute (Html.Attributes.attribute k v)


visibility : List (Visibility a) -> List Bool
visibility l =
    List.map isVisible l


visibility_presence : List (Maybe (Visibility a)) -> List Bool
visibility_presence l =
    List.map (\vo -> vo |> Maybe.map isVisible |> Maybe.withDefault False) l


init_two_points_in_diff : ( Float, Float ) -> ( Float, Float ) -> Main.Model
init_two_points_in_diff ( x, y ) ( j, i ) =
    { init_none | expected = Hidden [ [] ], actual = Hidden [ [] ], diff = Visible [ [ point x y, point j i ] ] }


init_none : Main.Model
init_none =
    Main.init Nothing |> Tuple.first


init_some : Main.Model
init_some =
    Main.init (just_flags test_flags) |> Tuple.first


type alias FlagContour =
    List (List ( Float, Float ))


type alias ExtraFlagContour =
    ( String, FlagContour )


test_flags : { image : Maybe { url : String, width : number, height : number, refX : Float, refY : Float, pixelWidth : Float, pixelHeight : Float }, expected : FlagContour, actual : FlagContour, diff : FlagContour, extras : List ExtraFlagContour }
test_flags =
    { image = Just { url = "test.jpg", width = 10, height = 20, refX = 0.0, refY = 0.0, pixelWidth = 1.0, pixelHeight = 1.0 }, expected = [ [] ], actual = [ [] ], diff = [ [] ], extras = [] }


init_some_no_image : Main.Model
init_some_no_image =
    Main.init (just_flags init_some_flags) |> Tuple.first


init_some_flags : { image : Maybe a, expected : FlagContour, actual : FlagContour, diff : FlagContour, extras : List b }
init_some_flags =
    { image = Nothing, expected = test_expected_contour, actual = test_actual_contour, diff = test_diff_contour, extras = [] }


test_expected_contour : FlagContour
test_expected_contour =
    [ base_contour, diff_expected ]


test_actual_contour : FlagContour
test_actual_contour =
    [ base_contour, diff_actual ]


test_diff_contour : FlagContour
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


just_flags : Init.Init -> Maybe Json.Encode.Value
just_flags f =
    Just <| Init.encode f
