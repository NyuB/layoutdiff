module Main_test exposing (..)

import Expect
import Main exposing (Msg(..), Triforce(..), update)
import Test exposing (..)
import Visibility exposing (..)


visibility : List (Visibility a) -> List Bool
visibility l =
    List.map isVisible l


init : Main.Model
init =
    let
        ( m, _ ) =
            Main.init Nothing
    in
    m


suite : Test
suite =
    describe "Main logic"
        [ describe "Visibility"
            [ test "Visibility at init" <|
                \_ -> Expect.equalLists [ True, True, True, False ] (visibility [ init.expected, init.actual, init.diff ] ++ visibility [ init.image ])
            , test "Toggle expected visibility" <|
                \_ ->
                    let
                        ( updated, _ ) =
                            update (ToggleContour Expected) init
                    in
                    Expect.equal updated.expected (Hidden (content init.expected))
            , test "Toggle actual visibility" <|
                \_ ->
                    let
                        ( updated, _ ) =
                            update (ToggleContour Actual) init
                    in
                    Expect.equal updated.actual (Hidden (content init.actual))
            , test "Toggle diff visibility" <|
                \_ ->
                    let
                        ( updated, _ ) =
                            update (ToggleContour Diff) init
                    in
                    Expect.equal updated.diff (Hidden (content init.diff))
            , test "Toggle image visibility" <|
                \_ ->
                    let
                        ( updated, _ ) =
                            update ToggleImage init
                    in
                    Expect.equal updated.image (Visible (content init.image))
            , test "Updating image url makes it visible" <|
                \_ ->
                    let
                        ( updated, _ ) =
                            update (ChangeImageUrl "test") init
                    in
                    Expect.equal updated.image (Visible "test")
            , test "If an image is passed to init flags, it is visible from start" <|
                \_ ->
                    let
                        ( init_model, _ ) =
                            Main.init (Just { imageUrl = "test.jpg" })
                    in
                    Expect.equal True (isVisible init_model.image)
            ]
        ]
