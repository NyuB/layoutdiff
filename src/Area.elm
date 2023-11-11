module Area exposing (Area, Point, ReferentialOrigin(..), ZoomedArea, expand_by, expand_for_point, full, horizontal_shift, initZoom, min_area, point, point_x, point_y, shift_origin, shrink_by, translate_point_to_referential, vertical_shift, zero, zoom, zoomed)


type Point
    = Point ( Float, Float )


{-| Rectangular area
-}
type alias Area =
    { origin : Point
    , width : Float
    , height : Float
    }


{-| Represents a zomm on a given original area
-}
type ZoomedArea
    = ZoomedArea { full : Area, zoomed : Area }


zoom : Float -> ZoomedArea -> ZoomedArea
zoom by (ZoomedArea z) =
    let
        transformation =
            if by < 0 then
                expand_by

            else
                shrink_by
    in
    ZoomedArea { full = z.full, zoomed = transformation by z.zoomed }


vertical_shift : Float -> ZoomedArea -> ZoomedArea
vertical_shift by (ZoomedArea z) =
    ZoomedArea { full = z.full, zoomed = shift_by_vertical by z.zoomed }


horizontal_shift : Float -> ZoomedArea -> ZoomedArea
horizontal_shift by (ZoomedArea z) =
    ZoomedArea { full = z.full, zoomed = shift_by_horizontal by z.zoomed }


zoomed : ZoomedArea -> Area
zoomed (ZoomedArea z) =
    z.zoomed


full : ZoomedArea -> Area
full (ZoomedArea z) =
    z.full


initZoom : Area -> ZoomedArea
initZoom area =
    ZoomedArea { full = area, zoomed = area }


{-|

  - TopLeft : x axis grows to the right, y axis grows downward
  - TopRight : x axis grows to the left, y axis grows downward
  - BottomLeft : x axis grows to the right, y axis grows upward
  - BottomRight : x axis grows to the left, y axis grows upward

-}
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


point : Float -> Float -> Point
point x y =
    Point ( x, y )


point_x : Point -> Float
point_x (Point ( x, _ )) =
    x


point_y : Point -> Float
point_y (Point ( _, y )) =
    y


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


{-| Expand area by a given margin on all four cardinal directions
-}
expand_by : Float -> Area -> Area
expand_by delta area =
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


{-| Shrink area by a given margin on all four cardinal directions
Return orignal area as is if shrinking by more than width (or height) / 2
-}
shrink_by : Float -> Area -> Area
shrink_by delta area =
    let
        dx =
            -(abs delta)

        dy =
            -(abs delta)

        ( ax, ay ) =
            ( point_x area.origin - dx, point_y area.origin - dy )

        ( w, h ) =
            ( area.width + 2 * dx, area.height + 2 * dy )
    in
    if w <= 0 || h <= 0 then
        area

    else
        { origin = point ax ay, width = w, height = h }


shift_by_horizontal : Float -> Area -> Area
shift_by_horizontal dx area =
    let
        x =
            point_x area.origin + dx
    in
    { origin = point x (point_y area.origin), width = area.width, height = area.height }


shift_by_vertical : Float -> Area -> Area
shift_by_vertical dy area =
    let
        y =
            point_y area.origin + dy
    in
    { origin = point (point_x area.origin) y, width = area.width, height = area.height }


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


translate_point_to_referential : Area -> { contourRef : ReferentialOrigin, targetRef : ReferentialOrigin } -> Point -> Point
translate_point_to_referential area ref =
    case ( ref.contourRef, ref.targetRef ) of
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

        ( TopRight, TopLeft ) ->
            revert_x area.width

        ( BottomLeft, BottomRight ) ->
            revert_x area.width

        ( BottomRight, BottomRight ) ->
            revert_x area.width

        ( a, b ) ->
            if a == b then
                identity

            else
                \p -> p |> revert_x area.width |> revert_y area.height



-- internals


revert_y : Float -> Point -> Point
revert_y maxY p =
    point (point_x p) (maxY - point_y p)


revert_x : Float -> Point -> Point
revert_x maxX p =
    point (maxX - point_x p) (point_y p)
