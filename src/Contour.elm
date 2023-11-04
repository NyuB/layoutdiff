module Contour exposing (Area, Contour, Point, contour_area, expand_by, expand_for_contour, expand_for_point, min_area, point, point_x, point_y, shift_origin, zero)

-- Rectangular area


type alias Area =
    { origin : Point
    , width : Float
    , height : Float
    }



-- Origin point


zero : Point
zero =
    Point ( 0, 0 )



-- Empty area


min_area : Area
min_area =
    { origin = zero, width = 0, height = 0 }



-- Expand area to contain  a given point


expand_for_point : Area -> Point -> Area
expand_for_point a (Point ( x, y )) =
    let
        ( ax, ay ) =
            ( point_x a.origin, point_y a.origin )

        lx =
            min x ax

        ly =
            min y ay

        rx =
            max x (ax + a.width)

        ry =
            max y (ay + a.height)
    in
    { origin = point lx ly, width = rx - lx, height = ry - ly }


expand_for_points : Area -> List Point -> Area
expand_for_points area pts =
    List.foldl (\p a -> expand_for_point a p) area pts


expand_for_contour : Area -> Contour -> Area
expand_for_contour area contour =
    List.foldl (\c a -> expand_for_points a c) area contour



-- Expand area by a given margin on all four cardinal directions


expand_by : Area -> Float -> Area
expand_by area delta =
    let
        dx =
            abs delta

        dy =
            abs delta

        ( ax, ay ) =
            ( point_x area.origin - dx, point_y area.origin - dy )

        ( w, h ) =
            ( area.width + 2 * dx, area.height + 2 * dy )
    in
    { origin = point ax ay, width = w, height = h }



-- Minimal area to contain all points of a given contour


contour_area : Contour -> Area
contour_area contour =
    List.foldl (\point_list area -> List.foldl (\p a -> expand_for_point a p) area point_list) min_area contour


type Point
    = Point ( Float, Float )


type alias Contour =
    List (List Point)


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
