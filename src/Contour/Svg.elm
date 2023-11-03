module Contour.Svg exposing (d, viewBox)

import Contour exposing (..)


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


viewBox : { width : Int, height : Int, xUnit : Float, yUnit : Float } -> String
viewBox img =
    let
        w =
            toFloat img.width * img.xUnit

        h =
            toFloat img.height * img.yUnit
    in
    [ 0, 0, w, h ] |> List.map String.fromFloat |> String.join " "
