module Area.Svg exposing (viewBox)

import Area exposing (Area)


{-| A suitable value for the viewbox atribute of an svg element corresponding to the given area
-}
viewBox : Area -> String
viewBox area =
    [ Area.point_x area.origin, Area.point_y area.origin, area.width, area.height ] |> List.map String.fromFloat |> String.join " "
