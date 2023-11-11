module Contour_test exposing (suite)

import Area exposing (Point)
import Contour exposing (contour_area)
import Contour.Svg
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Contour"
        [ describe "Area of contour" <|
            quick_tests
                [ square_area
                , triangle_area
                , square_triangle_contour_area
                ]
        , describe "SVG representation" <|
            quick_tests
                [ single_point_path
                , square_path
                ]
        ]


type alias Quick_test =
    ( String, () -> Expectation )


quick_test : ( String, () -> Expectation ) -> Test
quick_test ( name, t ) =
    test name t


quick_tests : List Quick_test -> List Test
quick_tests l =
    List.map quick_test l


square_area : Quick_test
square_area =
    ( "Square area", \_ -> Expect.equal { origin = Area.zero, width = 5, height = 5 } (contour_area [ square ]) )


triangle_area : Quick_test
triangle_area =
    ( "Triangle area", \_ -> Expect.equal { origin = Area.point -5 -3, width = 11, height = 7 } (contour_area [ triangle ]) )


square_triangle_contour_area : Quick_test
square_triangle_contour_area =
    ( "[square, triangle] area", \_ -> Expect.equal { origin = Area.point -5 -3, width = 11, height = 8 } (contour_area [ square, triangle ]) )


single_point_path : Quick_test
single_point_path =
    ( "Single point path", \_ -> Expect.equal "M 10 5" (Contour.Svg.d [ Area.point 10 5 ]) )


square_path : Quick_test
square_path =
    ( "Square path", \_ -> Expect.equal "M 0 0 L 5 0 L 5 5 L 0 5z" (Contour.Svg.d square) )


square : List Point
square =
    [ Area.point 0.0 0.0, Area.point 5.0 0.0, Area.point 5.0 5.0, Area.point 0.0 5.0 ]


triangle : List Point
triangle =
    [ Area.point -5 -2, Area.point 0 4, Area.point 6 -3 ]
