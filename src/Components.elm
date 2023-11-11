module Components exposing (CustomEvent, el_event, html_event, mouse_wheel_listener, referential_selector, svg_event)

import Area exposing (ReferentialOrigin(..))
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Events as EE
import Html
import Html.Events
import Json.Decode as Decode
import Svg
import Svg.Events



-- exposed


referential_selector : List (E.Attribute msg) -> ReferentialOrigin -> (ReferentialOrigin -> msg) -> E.Element msg
referential_selector attrs current onClick =
    E.row (attrs ++ [ E.height (E.px 100), E.width (E.px 100), E.spacing 5 ])
        [ E.el [ E.height E.fill, E.width (E.fillPortion 1) ] (referential_selector_left_column current onClick)
        , E.el [ E.height E.fill, E.width (E.fillPortion 1) ] referential_selector_mid_column
        , E.el [ E.height E.fill, E.width (E.fillPortion 1) ] (referential_selector_right_column current onClick)
        ]


type CustomEvent msg
    = CustomEvent (LabeledHandler msg)


mouse_wheel_listener : (Float -> msg) -> CustomEvent msg
mouse_wheel_listener f =
    let
        decoder =
            Decode.field "deltaY" Decode.float
                |> Decode.map f
                |> Decode.map (\z -> { message = z, preventDefault = True, stopPropagation = True })
    in
    CustomEvent ( "wheel", decoder )


svg_event : CustomEvent msg -> Svg.Attribute msg
svg_event (CustomEvent ( label, decoder )) =
    Svg.Events.custom label decoder


html_event : CustomEvent msg -> Html.Attribute msg
html_event (CustomEvent ( label, decoder )) =
    Html.Events.custom label decoder


el_event : CustomEvent msg -> E.Attribute msg
el_event custom =
    E.htmlAttribute (html_event custom)



-- internals


type alias LabeledHandler msg =
    ( String, Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool } )


referential_selector_left_column : ReferentialOrigin -> (ReferentialOrigin -> msg) -> E.Element msg
referential_selector_left_column current onClick =
    E.column [ E.height E.fill, E.width E.fill, E.spacing 5 ]
        [ referential_selector_item [ EB.roundEach { zeroRound | topLeft = 5 }, EB.widthEach { zeroWidth | top = 2, left = 2 } ] current onClick TopLeft
        , referential_selector_void
        , referential_selector_item [ EB.roundEach { zeroRound | bottomLeft = 5 }, EB.widthEach { zeroWidth | bottom = 2, left = 2 } ] current onClick BottomLeft
        ]


referential_selector_right_column : ReferentialOrigin -> (ReferentialOrigin -> msg) -> E.Element msg
referential_selector_right_column current onClick =
    E.column [ E.height E.fill, E.width E.fill, E.spacing 5 ]
        [ referential_selector_item [ EB.roundEach { zeroRound | topRight = 5 }, EB.widthEach { zeroWidth | top = 2, right = 2 } ] current onClick TopRight
        , referential_selector_void
        , referential_selector_item [ EB.roundEach { zeroRound | bottomRight = 5 }, EB.widthEach { zeroWidth | bottom = 2, right = 2 } ] current onClick BottomRight
        ]


referential_selector_mid_column : E.Element msg
referential_selector_mid_column =
    E.column [ E.height E.fill, E.width E.fill, E.spacing 5 ]
        [ referential_selector_void
        , referential_selector_void
        , referential_selector_void
        ]


colored_filled : E.Color -> E.Element msg
colored_filled c =
    E.el [ EBG.color c, E.width E.fill, E.height E.fill ] E.none


colored_filled_active : E.Color -> msg -> E.Element msg
colored_filled_active c onClick =
    E.el [ EBG.color c, E.width E.fill, E.height E.fill, EE.onClick onClick ] E.none


referential_selector_item : List (E.Attribute msg) -> ReferentialOrigin -> (ReferentialOrigin -> msg) -> ReferentialOrigin -> E.Element msg
referential_selector_item attrs current onClick position =
    let
        color =
            if current == position then
                E.rgb255 0 125 0

            else
                E.rgb255 0 0 125
    in
    E.el (attrs ++ [ E.padding 2, E.width E.fill, E.height (E.fillPortion 1) ]) (colored_filled_active color (onClick position))


referential_selector_void : E.Element msg
referential_selector_void =
    E.el [ E.padding 2, E.width E.fill, E.height (E.fillPortion 1) ] (colored_filled (E.rgba 0.7 0.7 0.7 0.5))


zeroRound : { topLeft : number, topRight : number, bottomLeft : number, bottomRight : number }
zeroRound =
    { topLeft = 0, topRight = 0, bottomLeft = 0, bottomRight = 0 }


zeroWidth : { top : number, left : number, bottom : number, right : number }
zeroWidth =
    { top = 0, left = 0, bottom = 0, right = 0 }
