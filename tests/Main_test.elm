module Main_test exposing (..)

import Expect
import Main exposing (Msg(..), Triforce(..), update)
import Test exposing (..)
import Visibility exposing (..)


suite : Test
suite =
    describe "Main logic"
        [ visibility_suite
        ]


visibility_suite : Test
visibility_suite =
    describe "Visibility"
        [ test "Visibility at init None" <|
            \_ -> Expect.equalLists [ False, False, False, False ] (visibility [ init_none.expected, init_none.actual, init_none.diff ] ++ visibility [ init_none.image ])
        , test "Visibility at init with flags" <|
            \_ -> Expect.equalLists [ True, True, True, True ] (visibility [ init_some.expected, init_some.actual, init_some.diff ] ++ visibility [ init_some.image ])
        , test "Toggle expected visibility" <|
            \_ ->
                let
                    ( updated, _ ) =
                        update (ToggleContour Expected) init_some
                in
                Expect.equal updated.expected (Hidden (content init_some.expected))
        , test "Toggle actual visibility" <|
            \_ ->
                let
                    ( updated, _ ) =
                        update (ToggleContour Actual) init_some
                in
                Expect.equal updated.actual (Hidden (content init_some.actual))
        , test "Toggle diff visibility" <|
            \_ ->
                let
                    ( updated, _ ) =
                        update (ToggleContour Diff) init_some
                in
                Expect.equal updated.diff (Hidden (content init_some.diff))
        , test "Toggle image visibility" <|
            \_ ->
                let
                    ( updated, _ ) =
                        update ToggleImage init_some
                in
                Expect.equal updated.image (Hidden (content init_some.image))
        , test "Updating image url makes it visible" <|
            \_ ->
                let
                    ( updated, _ ) =
                        update (ChangeImageUrl "test") init_none
                in
                Expect.equal True (isVisible updated.image)
        , test "If init flags are passed, components are visible from start" <|
            \_ ->
                Expect.equal True (isVisible init_some.image)
        ]


visibility : List (Visibility a) -> List Bool
visibility l =
    List.map isVisible l


init_none : Main.Model
init_none =
    let
        ( m, _ ) =
            Main.init Nothing
    in
    m


init_some : Main.Model
init_some =
    let
        ( m, _ ) =
            Main.init (Just test_flags)
    in
    m


test_flags : { image : { url : String, width : number, height : number, refX : Float, refY : Float, pixelWidth : Float, pixelHeight : Float }, expected : List (List a), actual : List (List b), diff : List (List c) }
test_flags =
    { image = { url = "test.jpg", width = 10, height = 20, refX = 0.0, refY = 0.0, pixelWidth = 1.0, pixelHeight = 1.0 }, expected = [ [] ], actual = [ [] ], diff = [ [] ] }
