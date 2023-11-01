module Contour exposing (Point, d, point, point_x, point_y, shift_origin)


type Point
    = Point ( Float, Float )


point : Float -> Float -> Point
point x y =
    Point ( x, y )


point_x : Point -> Float
point_x (Point ( x, _ )) =
    x


point_y : Point -> Float
point_y (Point ( _, y )) =
    y


shift_origin : Point -> Point -> Point -> Point
shift_origin from to =
    let
        delta_x =
            point_x to - point_x from

        delta_y =
            point_y to - point_y from
    in
    \p -> point (point_x p - delta_x) (point_y p - delta_y)


d_L : Point -> String
d_L p =
    "L " ++ String.fromFloat (point_x p) ++ " " ++ String.fromFloat (point_y p)


d_M : Point -> String
d_M p =
    "M " ++ String.fromFloat (point_x p) ++ " " ++ String.fromFloat (point_y p)


d : List Point -> String
d pts =
    case pts of
        [] ->
            ""

        single :: [] ->
            d_M single

        first :: rest ->
            d_M first ++ " " ++ String.join " " (List.map d_L rest)
