module Contour exposing (Contour, contour_area, expand_for_contour, translate_contour_to_referential)

import Area exposing (Area, Point, ReferentialOrigin)



-- exposed


{-| A contour expressed as a set of polygon
-}
type alias Contour =
    List (List Point)


{-| Expand area to contain the given points
-}
expand_for_contour : Area -> Contour -> Area
expand_for_contour area contour =
    List.foldl (\c a -> expand_for_points a c) area contour


{-| Minimal area to contain all points of a given contour
-}
contour_area : Contour -> Area
contour_area contour =
    List.foldl (\point_list area -> List.foldl (\p a -> Area.expand_for_point a p) area point_list) Area.min_area contour


{-| Convert the coordinates of contour assuming it was expressed from a referential having origin contourRef of area to a new referential where origin is at targetRef of area
-}
translate_contour_to_referential : Area -> { contourRef : ReferentialOrigin, targetRef : ReferentialOrigin } -> Contour -> Contour
translate_contour_to_referential area ref contour =
    let
        translation =
            Area.translate_point_to_referential area ref
    in
    List.map (\pts -> List.map translation pts) contour



-- internals


expand_for_points : Area -> List Point -> Area
expand_for_points area pts =
    List.foldl (\p a -> Area.expand_for_point a p) area pts
