module Main_test exposing (..)

import Expect
import Main exposing (Msg(..), Triforce(..), Visibility(..), content, init, update, visible)
import Test exposing (..)


suite : Test
suite =
    describe "Main logic"
        [ describe "Init"
            [ test "All visible at init" <|
                \_ -> Expect.equalLists [ True, True, True ] (List.map visible [ init.expected, init.actual, init.diff ])
            ]
        , describe "Toggle"
            [ test "Revert expected visibility" <|
                \_ ->
                    let
                        updated =
                            update (ToggleContour Expected) init
                    in
                    Expect.equal updated.expected (Hidden (content init.expected))
            , test "Revert actual visibility" <|
                \_ ->
                    let
                        updated =
                            update (ToggleContour Actual) init
                    in
                    Expect.equal updated.actual (Hidden (content init.actual))
            , test "Revert diff visibility" <|
                \_ ->
                    let
                        updated =
                            update (ToggleContour Diff) init
                    in
                    Expect.equal updated.diff (Hidden (content init.diff))
            ]
        ]
