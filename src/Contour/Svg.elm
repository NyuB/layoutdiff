module Contour.Svg exposing (d, viewBox)

import Contour exposing (..)



-- exposed


{-| A suitable value for the d attribute of an svg path corresponding to the given points
-}
d : List Point -> String
d points =
    case points of
        [] ->
            ""

        single :: [] ->
            d_M single

        first :: rest ->
            d_M first ++ " " ++ String.join " " (List.map d_L rest) |> closed


{-| A suitable value for the viewbox atribute of an svg element corresponding to the given area
-}
viewBox : Area -> String
viewBox area =
    [ point_x area.origin, point_y area.origin, area.width, area.height ] |> List.map String.fromFloat |> String.join " "



-- internals


d_L : Point -> String
d_L p =
    "L " ++ String.fromFloat (point_x p) ++ " " ++ String.fromFloat (point_y p)


d_M : Point -> String
d_M p =
    "M " ++ String.fromFloat (point_x p) ++ " " ++ String.fromFloat (point_y p)


closed path_str =
    if String.endsWith "z" path_str then
        path_str

    else
        path_str ++ "z"
