module Components_test exposing (suite)

import Components exposing (MouseDragEvent(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Components"
        [ describe "Mouse drag"
            [ test "Click, drag left, release" <|
                \_ -> Expect.equalLists [ ( -1, 0 ) ] (drag_play [ Clicked ( 0, 0 ), Moved ( -1, 0 ), Released ( -1, 0 ) ])
            , test "Click, release" <|
                \_ -> Expect.equalLists [] (drag_play [ Clicked ( 0, 0 ), Released ( 0, 0 ) ])
            , test "Click, drag up" <|
                \_ -> Expect.equalLists [ ( 0, 1 ) ] (drag_play [ Clicked ( 0, 0 ), Moved ( 0, 1 ) ])
            , test "Click, drag downright" <|
                \_ -> Expect.equalLists [ ( 1, -1 ) ] (drag_play [ Clicked ( 0, 0 ), Moved ( 1, -1 ) ])
            , test "Move only" <|
                \_ -> Expect.equalLists [] (drag_play [ Moved ( 1, -1 ) ])
            , test "Drag twice, release, move" <|
                \_ -> Expect.equalLists [ ( 0, 1 ), ( 1, 0 ) ] (drag_play [ Clicked ( 1, 1 ), Moved ( 1, 2 ), Moved ( 2, 2 ), Released ( 2, 2 ), Moved ( 42, 24 ) ])
            ]
        ]


drag_play : List MouseDragEvent -> List ( Float, Float )
drag_play events =
    events
        |> List.foldl
            (\evt ( tracker, result ) ->
                let
                    ( nextTracker, move ) =
                        Components.updateDrag evt tracker
                in
                ( nextTracker, move :: result )
            )
            ( Components.initMouseDragTracker, [] )
        |> Tuple.second
        |> List.filterMap identity
        |> List.reverse
