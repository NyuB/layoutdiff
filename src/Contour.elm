module Contour exposing (Area, Contour, Point, ReferentialOrigin(..), contour_area, expand_by, expand_for_contour, expand_for_point, min_area, point, point_x, point_y, shift_origin, translate_contour_to_referential, zero)


type Point
    = Point ( Float, Float )


{-| A contour expressed as a set of polygon
-}
type alias Contour =
    List (List Point)


{-| Rectangular area
-}
type alias Area =
    { origin : Point
    , width : Float
    , height : Float
    }


type ReferentialOrigin
    = TopLeft
    | BottomLeft
    | TopRight
    | BottomRight


{-| (0,0)
-}
zero : Point
zero =
    Point ( 0, 0 )


{-| Empty area
-}
min_area : Area
min_area =
    { origin = zero, width = 0, height = 0 }


{-| Expand area to contain a given point
-}
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


{-| Expand area to contain the given points
-}
expand_for_points : Area -> List Point -> Area
expand_for_points area pts =
    List.foldl (\p a -> expand_for_point a p) area pts


{-| Expand area to contain the given points
-}
expand_for_contour : Area -> Contour -> Area
expand_for_contour area contour =
    List.foldl (\c a -> expand_for_points a c) area contour


{-| Expand area by a given margin on all four cardinal directions
-}
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


{-| Minimal area to contain all points of a given contour
-}
contour_area : Contour -> Area
contour_area contour =
    List.foldl (\point_list area -> List.foldl (\p a -> expand_for_point a p) area point_list) min_area contour


point : Float -> Float -> Point
point x y =
    Point ( x, y )


point_x : Point -> Float
point_x (Point ( x, _ )) =
    x


point_y : Point -> Float
point_y (Point ( _, y )) =
    y


{-| express the Point p in the new referential described by target, assuming p was expressed in the referential origin

        shift_origin (point 0 0) (point 1 2) (point 5 5) == point 4 3

-}
shift_origin : Point -> Point -> Point -> Point
shift_origin origin target p =
    let
        delta_x =
            point_x target - point_x origin

        delta_y =
            point_y target - point_y origin
    in
    point (point_x p - delta_x) (point_y p - delta_y)


revert_y : Float -> Point -> Point
revert_y maxY p =
    point (point_x p) (maxY - point_y p)


revert_x : Float -> Point -> Point
revert_x maxX p =
    point (maxX - point_x p) (point_y p)


translate_point_to_referential : Area -> { contourRef : ReferentialOrigin, targetRef : ReferentialOrigin } -> Point -> Point
translate_point_to_referential area ref =
    case ( ref.contourRef, ref.targetRef ) of
        ( TopLeft, TopLeft ) ->
            identity

        ( BottomLeft, BottomLeft ) ->
            identity

        ( TopRight, TopRight ) ->
            identity

        ( BottomRight, BottomRight ) ->
            identity

        ( TopLeft, BottomLeft ) ->
            revert_y area.height

        ( BottomLeft, TopLeft ) ->
            revert_y area.height

        ( TopRight, BottomRight ) ->
            revert_y area.height

        ( BottomRight, TopRight ) ->
            revert_y area.height

        ( TopLeft, TopRight ) ->
            revert_x area.width

        ( BottomLeft, BottomRight ) ->
            revert_x area.width

        ( _, _ ) ->
            \p -> p |> revert_x area.width |> revert_y area.height


translate_contour_to_referential : Area -> { contourRef : ReferentialOrigin, targetRef : ReferentialOrigin } -> Contour -> Contour
translate_contour_to_referential area ref contour =
    let
        translation =
            translate_point_to_referential area ref
    in
    List.map (\pts -> List.map translation pts) contour
