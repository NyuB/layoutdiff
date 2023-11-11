module Area_test exposing (suite)

import Area exposing (Point, ReferentialOrigin(..))
import Area.Svg
import Contour exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Contour"
        [ describe "Point translation" <|
            quick_tests
                [ x_right_translation
                , x_left_translation
                , y_up_translation
                , y_down_translation
                , xy_diagonal_translation
                ]
        , describe "Area transformation" <|
            quick_tests
                [ translate_top_left_to_bot_left
                , translate_bot_left_to_top_left
                , translate_bot_left_to_top_right
                , translate_top_left_to_top_right
                , expand_by_empty_area
                , expand_by_rectangle_area
                , shrink_by_rectangle_area
                , shrink_by_too_small_area_x
                , shrink_by_too_small_area_y
                ]
        , describe "SVG representation" <|
            quick_tests
                [ view_box
                , view_box_skewed
                ]
        , describe "Zoom" <|
            quick_tests
                [ positive_zoom_reduces_area, negative_zoom_expands_area, same_full_after_zoom ]
        ]


square : List Point
square =
    [ Area.point 0.0 0.0, Area.point 5.0 0.0, Area.point 5.0 5.0, Area.point 0.0 5.0 ]


shift_all : (Point -> Point) -> List Point -> List Point
shift_all s pts =
    List.map s pts


type alias Quick_test =
    ( String, () -> Expectation )


quick_test : ( String, () -> Expectation ) -> Test
quick_test ( name, t ) =
    test name t


quick_tests : List Quick_test -> List Test
quick_tests l =
    List.map quick_test l


x_right_translation : Quick_test
x_right_translation =
    ( "Right translation"
    , \_ ->
        let
            shift_right =
                Area.shift_origin Area.zero (Area.point 0.5 0.0)

            shifted_square =
                shift_all shift_right square
        in
        Expect.equalLists [ Area.point -0.5 0, Area.point 4.5 0, Area.point 4.5 5, Area.point -0.5 5 ] shifted_square
    )


x_left_translation : Quick_test
x_left_translation =
    ( "Left translation"
    , \_ ->
        let
            shift_left =
                Area.shift_origin Area.zero (Area.point -0.5 0.0)

            shifted_square =
                shift_all shift_left square
        in
        Expect.equalLists [ Area.point 0.5 0, Area.point 5.5 0, Area.point 5.5 5, Area.point 0.5 5 ] shifted_square
    )


y_up_translation : Quick_test
y_up_translation =
    ( "Up translation"
    , \_ ->
        let
            shift_up =
                Area.shift_origin Area.zero (Area.point 0 1)

            shifted_square =
                shift_all shift_up square
        in
        Expect.equalLists [ Area.point 0 -1, Area.point 5 -1, Area.point 5 4, Area.point 0 4 ] shifted_square
    )


y_down_translation : Quick_test
y_down_translation =
    ( "Down translation"
    , \_ ->
        let
            shift_down =
                Area.shift_origin Area.zero (Area.point 0 -1)

            shifted_square =
                shift_all shift_down square
        in
        Expect.equalLists [ Area.point 0 1, Area.point 5 1, Area.point 5 6, Area.point 0 6 ] shifted_square
    )


xy_diagonal_translation : Quick_test
xy_diagonal_translation =
    ( "Diagonal translation"
    , \_ ->
        let
            shift_left =
                Area.shift_origin Area.zero (Area.point 1 -2)

            shifted_square =
                shift_all shift_left square
        in
        Expect.equalLists [ Area.point -1 2, Area.point 4 2, Area.point 4 7, Area.point -1 7 ] shifted_square
    )


translate_top_left_to_bot_left : Quick_test
translate_top_left_to_bot_left =
    ( "TopLeft -> BottomLeft"
    , \_ ->
        let
            triangle_origin =
                [ [ Area.point 0 0, Area.point 0 4, Area.point 2 2 ] ]

            targetArea =
                { origin = Area.zero, width = 10, height = 5 }
        in
        Expect.equal [ [ Area.point 0 5, Area.point 0 1, Area.point 2 3 ] ] (translate_contour_to_referential targetArea { contourRef = TopLeft, targetRef = BottomLeft } triangle_origin)
    )


translate_bot_left_to_top_left : Quick_test
translate_bot_left_to_top_left =
    ( "BottomLeft -> TopLeft"
    , \_ ->
        let
            triangle_origin =
                [ [ Area.point 0 0, Area.point 0 4, Area.point 2 2 ] ]

            targetArea =
                { origin = Area.zero, width = 10, height = 5 }
        in
        Expect.equal [ [ Area.point 0 5, Area.point 0 1, Area.point 2 3 ] ] (translate_contour_to_referential targetArea { contourRef = BottomLeft, targetRef = TopLeft } triangle_origin)
    )


translate_bot_left_to_top_right : Quick_test
translate_bot_left_to_top_right =
    ( "BottomLeft -> TopRight"
    , \_ ->
        let
            triangle_origin =
                [ [ Area.point 0 0, Area.point 0 4, Area.point 2 2 ] ]

            targetArea =
                { origin = Area.zero, width = 10, height = 5 }
        in
        Expect.equal [ [ Area.point 10 5, Area.point 10 1, Area.point 8 3 ] ] (translate_contour_to_referential targetArea { contourRef = BottomLeft, targetRef = TopRight } triangle_origin)
    )


translate_top_left_to_top_right : Quick_test
translate_top_left_to_top_right =
    ( "TopLeft -> TopRight"
    , \_ ->
        let
            triangle_origin =
                [ [ Area.point 0 0, Area.point 0 4, Area.point 2 2 ] ]

            targetArea =
                { origin = Area.zero, width = 10, height = 5 }
        in
        Expect.equal [ [ Area.point 10 0, Area.point 10 4, Area.point 8 2 ] ] (translate_contour_to_referential targetArea { contourRef = TopLeft, targetRef = TopRight } triangle_origin)
    )


expand_by_empty_area : Quick_test
expand_by_empty_area =
    ( "Expand an empty area"
    , \_ ->
        let
            expected =
                { origin = Area.point -4.5 -4.5, width = 9, height = 9 }
        in
        Expect.equal expected (Area.expand_by 4.5 Area.min_area)
    )


expand_by_rectangle_area : Quick_test
expand_by_rectangle_area =
    ( "Expand a rectangular area"
    , \_ ->
        let
            original =
                { origin = Area.point -5 -10, width = 10, height = 20 }

            delta =
                5.25

            expected =
                { origin = Area.point -10.25 -15.25, width = 20.5, height = 30.5 }
        in
        Expect.equal expected (Area.expand_by delta original)
    )


shrink_by_too_small_area_x : Quick_test
shrink_by_too_small_area_x =
    ( "Shrink an area smaller than shrink delta in x"
    , \_ ->
        let
            original =
                { origin = Area.point -5 -10, width = 10, height = 20 }

            delta =
                10
        in
        Expect.equal original (Area.shrink_by delta original)
    )


shrink_by_too_small_area_y : Quick_test
shrink_by_too_small_area_y =
    ( "Shrink an area smaller than shrink delta in y"
    , \_ ->
        let
            original =
                { origin = Area.point -5 -10, width = 20, height = 10 }

            delta =
                6
        in
        Expect.equal original (Area.shrink_by delta original)
    )


shrink_by_rectangle_area : Quick_test
shrink_by_rectangle_area =
    ( "Shrink a rectangular area"
    , \_ ->
        let
            original =
                { origin = Area.point -5 -10, width = 10, height = 20 }

            delta =
                3.25

            expected =
                { origin = Area.point -1.75 -6.75, width = 3.5, height = 13.5 }
        in
        Expect.equal expected (Area.shrink_by delta original)
    )


view_box : Quick_test
view_box =
    ( "SVG viewbox", \_ -> Expect.equal "0 0 4.5 7" (Area.Svg.viewBox { width = 4.5, height = 7, origin = Area.point 0 0 }) )


view_box_skewed : Quick_test
view_box_skewed =
    ( "SVG viewbox different origin", \_ -> Expect.equal "1 2 10 7.25" (Area.Svg.viewBox { width = 10, height = 7.25, origin = Area.point 1 2 }) )


positive_zoom_reduces_area : Quick_test
positive_zoom_reduces_area =
    ( "Positive zoom reduces the area"
    , \_ ->
        let
            original =
                Area.initZoom { origin = Area.point 0 0, width = 10, height = 10 }

            zoom_by =
                1

            expected =
                { origin = Area.point 1 1, width = 8, height = 8 }
        in
        Expect.equal expected (Area.zoom zoom_by original |> Area.zoomed)
    )


negative_zoom_expands_area : Quick_test
negative_zoom_expands_area =
    ( "Negative zoom expands the area"
    , \_ ->
        let
            original =
                Area.initZoom { origin = Area.point 0 0, width = 10, height = 10 }

            zoom_by =
                -1

            expected =
                { origin = Area.point -1 -1, width = 12, height = 12 }
        in
        Expect.equal expected (Area.zoom zoom_by original |> Area.zoomed)
    )


same_full_after_zoom : Quick_test
same_full_after_zoom =
    ( "Full area can be retrieved unmodified after zoom"
    , \_ ->
        let
            original =
                Area.initZoom { origin = Area.point 0 0, width = 10, height = 10 }

            zoom_by =
                3
        in
        Expect.equal (Area.full original) (Area.zoom zoom_by original |> Area.full)
    )
