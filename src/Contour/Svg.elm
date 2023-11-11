module Contour.Svg exposing (d)

import Area exposing (Point)
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



-- internals


d_L : Point -> String
d_L p =
    "L " ++ String.fromFloat (Area.point_x p) ++ " " ++ String.fromFloat (Area.point_y p)


d_M : Point -> String
d_M p =
    "M " ++ String.fromFloat (Area.point_x p) ++ " " ++ String.fromFloat (Area.point_y p)


closed : String -> String
closed path_str =
    if String.endsWith "z" path_str then
        path_str

    else
        path_str ++ "z"
