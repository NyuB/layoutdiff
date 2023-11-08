module Contour_test exposing (suite)

import Contour exposing (..)
import Contour.Svg
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
        , describe "Area" <|
            quick_tests
                [ square_area
                , triangle_area
                , square_triangle_contour_area
                , translate_top_left_to_bot_left
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
                [ single_point_path
                , square_path
                , view_box
                , view_box_skewed
                ]
        ]


square : List Point
square =
    [ point 0.0 0.0, point 5.0 0.0, point 5.0 5.0, point 0.0 5.0 ]


triangle : List Point
triangle =
    [ point -5 -2, point 0 4, point 6 -3 ]


shift_all : (a -> b) -> List a -> List b
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
                shift_origin zero (point 0.5 0.0)

            shifted_square =
                shift_all shift_right square
        in
        Expect.equalLists [ point -0.5 0, point 4.5 0, point 4.5 5, point -0.5 5 ] shifted_square
    )


x_left_translation : Quick_test
x_left_translation =
    ( "Left translation"
    , \_ ->
        let
            shift_left =
                shift_origin zero (point -0.5 0.0)

            shifted_square =
                shift_all shift_left square
        in
        Expect.equalLists [ point 0.5 0, point 5.5 0, point 5.5 5, point 0.5 5 ] shifted_square
    )


y_up_translation : Quick_test
y_up_translation =
    ( "Up translation"
    , \_ ->
        let
            shift_up =
                shift_origin zero (point 0 1)

            shifted_square =
                shift_all shift_up square
        in
        Expect.equalLists [ point 0 -1, point 5 -1, point 5 4, point 0 4 ] shifted_square
    )


y_down_translation : Quick_test
y_down_translation =
    ( "Down translation"
    , \_ ->
        let
            shift_down =
                shift_origin zero (point 0 -1)

            shifted_square =
                shift_all shift_down square
        in
        Expect.equalLists [ point 0 1, point 5 1, point 5 6, point 0 6 ] shifted_square
    )


xy_diagonal_translation : Quick_test
xy_diagonal_translation =
    ( "Diagonal translation"
    , \_ ->
        let
            shift_left =
                shift_origin zero (point 1 -2)

            shifted_square =
                shift_all shift_left square
        in
        Expect.equalLists [ point -1 2, point 4 2, point 4 7, point -1 7 ] shifted_square
    )


square_area : Quick_test
square_area =
    ( "Square area", \_ -> Expect.equal { origin = zero, width = 5, height = 5 } (contour_area [ square ]) )


triangle_area : Quick_test
triangle_area =
    ( "Triangle area", \_ -> Expect.equal { origin = point -5 -3, width = 11, height = 7 } (contour_area [ triangle ]) )


square_triangle_contour_area : Quick_test
square_triangle_contour_area =
    ( "[square, triangle] area", \_ -> Expect.equal { origin = point -5 -3, width = 11, height = 8 } (contour_area [ square, triangle ]) )


translate_top_left_to_bot_left : Quick_test
translate_top_left_to_bot_left =
    ( "TopLeft -> BottomLeft"
    , \_ ->
        let
            triangle_origin =
                [ [ point 0 0, point 0 4, point 2 2 ] ]

            targetArea =
                { origin = zero, width = 10, height = 5 }
        in
        Expect.equal [ [ point 0 5, point 0 1, point 2 3 ] ] (translate_contour_to_referential targetArea { contourRef = TopLeft, targetRef = BottomLeft } triangle_origin)
    )


translate_bot_left_to_top_left : Quick_test
translate_bot_left_to_top_left =
    ( "BottomLeft -> TopLeft"
    , \_ ->
        let
            triangle_origin =
                [ [ point 0 0, point 0 4, point 2 2 ] ]

            targetArea =
                { origin = zero, width = 10, height = 5 }
        in
        Expect.equal [ [ point 0 5, point 0 1, point 2 3 ] ] (translate_contour_to_referential targetArea { contourRef = BottomLeft, targetRef = TopLeft } triangle_origin)
    )


translate_bot_left_to_top_right : Quick_test
translate_bot_left_to_top_right =
    ( "BottomLeft -> TopRight"
    , \_ ->
        let
            triangle_origin =
                [ [ point 0 0, point 0 4, point 2 2 ] ]

            targetArea =
                { origin = zero, width = 10, height = 5 }
        in
        Expect.equal [ [ point 10 5, point 10 1, point 8 3 ] ] (translate_contour_to_referential targetArea { contourRef = BottomLeft, targetRef = TopRight } triangle_origin)
    )


translate_top_left_to_top_right : Quick_test
translate_top_left_to_top_right =
    ( "TopLeft -> TopRight"
    , \_ ->
        let
            triangle_origin =
                [ [ point 0 0, point 0 4, point 2 2 ] ]

            targetArea =
                { origin = zero, width = 10, height = 5 }
        in
        Expect.equal [ [ point 10 0, point 10 4, point 8 2 ] ] (translate_contour_to_referential targetArea { contourRef = TopLeft, targetRef = TopRight } triangle_origin)
    )


expand_by_empty_area : Quick_test
expand_by_empty_area =
    ( "Expand an empty area"
    , \_ ->
        let
            expected =
                { origin = point -4.5 -4.5, width = 9, height = 9 }
        in
        Expect.equal expected (expand_by 4.5 min_area)
    )


expand_by_rectangle_area : Quick_test
expand_by_rectangle_area =
    ( "Expand a rectangular area"
    , \_ ->
        let
            original =
                { origin = point -5 -10, width = 10, height = 20 }

            delta =
                5.25

            expected =
                { origin = point -10.25 -15.25, width = 20.5, height = 30.5 }
        in
        Expect.equal expected (expand_by delta original)
    )


shrink_by_too_small_area_x : Quick_test
shrink_by_too_small_area_x =
    ( "Shrink an area smaller than shrink delta in x"
    , \_ ->
        let
            original =
                { origin = point -5 -10, width = 10, height = 20 }

            delta =
                6
        in
        Expect.equal min_area (shrink_by delta original)
    )


shrink_by_too_small_area_y : Quick_test
shrink_by_too_small_area_y =
    ( "Shrink an area smaller than shrink delta in y"
    , \_ ->
        let
            original =
                { origin = point -5 -10, width = 20, height = 10 }

            delta =
                6
        in
        Expect.equal min_area (shrink_by delta original)
    )


shrink_by_rectangle_area : Quick_test
shrink_by_rectangle_area =
    ( "Shrink a rectangular area"
    , \_ ->
        let
            original =
                { origin = point -5 -10, width = 10, height = 20 }

            delta =
                3.25

            expected =
                { origin = point -1.75 -6.75, width = 3.5, height = 13.5 }
        in
        Expect.equal expected (shrink_by delta original)
    )


single_point_path : Quick_test
single_point_path =
    ( "Single point path", \_ -> Expect.equal "M 10 5" (Contour.Svg.d [ point 10 5 ]) )


square_path : Quick_test
square_path =
    ( "Square path", \_ -> Expect.equal "M 0 0 L 5 0 L 5 5 L 0 5" (Contour.Svg.d square) )


view_box : Quick_test
view_box =
    ( "SVG viewbox", \_ -> Expect.equal "0 0 4.5 7" (Contour.Svg.viewBox { width = 4.5, height = 7, origin = point 0 0 }) )


view_box_skewed : Quick_test
view_box_skewed =
    ( "SVG viewbox different origin", \_ -> Expect.equal "1 2 10 7.25" (Contour.Svg.viewBox { width = 10, height = 7.25, origin = point 1 2 }) )
