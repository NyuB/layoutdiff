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
            quick_tests [ square_area, triangle_area ]
        , describe "SVG representation" <|
            quick_tests [ single_point_path, square_path ]
        ]


square : List Point
square =
    [ point 0.0 0.0, point 5.0 0.0, point 5.0 5.0, point 0.0 5.0 ]


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
    ( "Triangle area", \_ -> Expect.equal { origin = point -5 -3, width = 11, height = 7 } (contour_area [ [ point -5 -2, point 0 4, point 6 -3 ] ]) )


single_point_path : Quick_test
single_point_path =
    ( "Single point path", \_ -> Expect.equal "M 10 5" (Contour.Svg.d [ point 10 5 ]) )


square_path : Quick_test
square_path =
    ( "Square path", \_ -> Expect.equal "M 0 0 L 5 0 L 5 5 L 0 5" (Contour.Svg.d square) )
