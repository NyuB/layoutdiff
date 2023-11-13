module Components exposing (CustomEvent, DragMove, MouseDragEvent(..), MouseDragTracker, initMouseDragTracker, mouse_drag_listener, mouse_wheel_listener, referential_selector, svg_event, updateDrag)

import Area exposing (ReferentialOrigin(..))
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Events as EE
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
mouse_wheel_listener handler =
    let
        decoder =
            Decode.field "deltaY" Decode.float
                |> Decode.map handler
                |> Decode.map (\z -> { message = z, preventDefault = True, stopPropagation = True })
    in
    CustomEvent ( "wheel", decoder )


mouse_drag_listener : MouseDragTracker -> (MouseDragTracker -> Maybe DragMove -> msg) -> List (CustomEvent msg)
mouse_drag_listener tracker handler =
    let
        tupled_handler ( x, y ) =
            handler x y
    in
    [ mouseDragDownListener, mouseDragMoveListener, mouseDragUpListener, mouseDragLeaveListener ]
        |> List.map (\listener -> listener tracker tupled_handler)


{-| A drag movement represented by (deltaX, deltaY) vector
-}
type alias DragMove =
    ( Float, Float )


{-| Mouse drag status
-}
type MouseDragTracker
    = Idle
    | Pressed ( Float, Float )


initMouseDragTracker : MouseDragTracker
initMouseDragTracker =
    Idle


{-| Simplified mouse event wrapper to implement drag behavior

Clicked -> Mouse button down

Released -> Mouse button up

Moved -> Mouse moved (up or down)

-}
type MouseDragEvent
    = Clicked ( Float, Float )
    | Released ( Float, Float )
    | Moved ( Float, Float )


{-| Update the current dragging status, possibly yielding a dragging move
-}
updateDrag : MouseDragEvent -> MouseDragTracker -> ( MouseDragTracker, Maybe DragMove )
updateDrag event tracker =
    case ( tracker, event ) of
        ( Idle, Clicked position ) ->
            ( Pressed position, Nothing )

        ( Idle, _ ) ->
            ( Idle, Nothing )

        ( Pressed before, Moved after ) ->
            ( Pressed after, moveIfDifference before after )

        ( Pressed before, Released after ) ->
            ( Idle, moveIfDifference before after )

        ( Pressed _, Clicked after ) ->
            ( Pressed after, Nothing )


svg_event : CustomEvent msg -> Svg.Attribute msg
svg_event (CustomEvent ( label, decoder )) =
    Svg.Events.custom label decoder



-- internals


type alias LabeledHandler msg =
    ( String, Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool } )


mouseXYDecoder : (( Float, Float ) -> a) -> Decode.Decoder a
mouseXYDecoder f =
    Decode.map2 (\x y -> f ( x, y )) (Decode.field "x" Decode.float) (Decode.field "y" Decode.float)


mouseDragListener : MouseDragTracker -> (( MouseDragTracker, Maybe DragMove ) -> msg) -> String -> (( Float, Float ) -> MouseDragEvent) -> CustomEvent msg
mouseDragListener tracker f eventType handler =
    let
        decoder =
            mouseXYDecoder handler
                |> Decode.map (\e -> updateDrag e tracker)
                |> Decode.map (\msg -> { message = f msg, stopPropagation = True, preventDefault = True })
    in
    CustomEvent ( eventType, decoder )


mouseDragDownListener : MouseDragTracker -> (( MouseDragTracker, Maybe DragMove ) -> msg) -> CustomEvent msg
mouseDragDownListener tracker f =
    mouseDragListener tracker f "mousedown" Clicked


mouseDragUpListener : MouseDragTracker -> (( MouseDragTracker, Maybe DragMove ) -> msg) -> CustomEvent msg
mouseDragUpListener tracker f =
    mouseDragListener tracker f "mouseup" Released


mouseDragLeaveListener : MouseDragTracker -> (( MouseDragTracker, Maybe DragMove ) -> msg) -> CustomEvent msg
mouseDragLeaveListener tracker f =
    mouseDragListener tracker f "mouseleave" Released


mouseDragMoveListener : MouseDragTracker -> (( MouseDragTracker, Maybe DragMove ) -> msg) -> CustomEvent msg
mouseDragMoveListener tracker f =
    mouseDragListener tracker f "mousemove" Moved


moveIfDifference : ( Float, Float ) -> ( Float, Float ) -> Maybe ( Float, Float )
moveIfDifference ( x, y ) ( a, b ) =
    if a /= x || b /= y then
        Just ( a - x, b - y )

    else
        Nothing


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
