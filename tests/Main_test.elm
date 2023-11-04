module Main_test exposing (..)

import Expect
import Html exposing (Html)
import Html.Attributes
import Main exposing (Msg(..), Triforce(..), update, view)
import Test exposing (..)
import Test.Html.Query as HQ
import Test.Html.Selector exposing (..)
import Visibility exposing (..)


suite : Test
suite =
    describe "Main logic"
        [ visibility_suite
        ]


html_suite : Test
html_suite =
    describe "Html rendering"
        [ svg_has_size_of_image
        ]


visibility_suite : Test
visibility_suite =
    describe "Visibility"
        [ test "Visibility at init None" <|
            \_ -> Expect.equalLists [ False, False, False, False ] (visibility [ init_none.expected, init_none.actual, init_none.diff ] ++ visibility_presence [ init_none.image ])
        , test "Visibility at init with flags" <|
            \_ -> Expect.equalLists [ True, True, True, True ] (visibility [ init_some.expected, init_some.actual, init_some.diff ] ++ visibility_presence [ init_some.image ])
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
                    initial =
                        init_some

                    ( updated, _ ) =
                        update ToggleImage initial
                in
                Expect.equal [ True, False ] (visibility_presence [ initial.image, updated.image ])
        , test "Updating image url makes it visible" <|
            \_ ->
                let
                    initial =
                        init_some |> update ToggleImage |> Tuple.first

                    ( updated, _ ) =
                        update (ChangeImageUrl "test") initial
                in
                Expect.equal [ False, True ] (visibility_presence [ initial.image, updated.image ])
        , test "If init flags are passed, components are visible from start" <|
            \_ ->
                Expect.equal [ True ] (visibility_presence [ init_some.image ])
        ]


single_svg : Html msg -> HQ.Single msg
single_svg v =
    v |> HQ.fromHtml |> HQ.find [ tag "svg" ]


svg_has_size_of_image : Test
svg_has_size_of_image =
    test "SVG window has same dimensions than image" <|
        \_ ->
            let
                v =
                    view init_some
            in
            single_svg v |> HQ.has [ html_attribute "viewBox" "0 0 10 20", html_attribute "width" "10", html_attribute "height" "20" ]


html_attribute : String -> String -> Selector
html_attribute k v =
    attribute (Html.Attributes.attribute k v)


visibility : List (Visibility a) -> List Bool
visibility l =
    List.map isVisible l


visibility_presence : List (Maybe (Visibility a)) -> List Bool
visibility_presence l =
    List.map (\vo -> vo |> Maybe.map isVisible |> Maybe.withDefault False) l


init_none : Main.Model
init_none =
    Main.init Nothing |> Tuple.first


init_some : Main.Model
init_some =
    Main.init (Just test_flags) |> Tuple.first


test_flags : { image : Maybe { url : String, width : number, height : number, refX : Float, refY : Float, pixelWidth : Float, pixelHeight : Float }, expected : List (List a), actual : List (List b), diff : List (List c) }
test_flags =
    { image = Just { url = "test.jpg", width = 10, height = 20, refX = 0.0, refY = 0.0, pixelWidth = 1.0, pixelHeight = 1.0 }, expected = [ [] ], actual = [ [] ], diff = [ [] ] }
