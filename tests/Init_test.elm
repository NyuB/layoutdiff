module Init_test exposing (suite)

import Expect
import Fuzz
import Init exposing (ImageSpec, Init, decode, encode)
import Json.Decode
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Init encoding/decoding"
        [ fuzz forall_init "Decode o Encode <=> identity" <|
            identity_modulo_ok encode_then_decode
        , test "Json nominal" <|
            \_ -> Expect.equal (Ok decoded_nominal) (raw_decode nominal)
        , test "Json nominal with unknwon additional fields" <|
            \_ -> Expect.equal (Ok decoded_nominal) (raw_decode valid_with_unknown_fields)
        , test "Json without image is valid" <|
            \_ -> raw_decode valid_without_image |> Result.map (\i -> i.image) |> Expect.equal (Ok Nothing)
        , test "Json without extra is valid" <|
            \_ -> raw_decode valid_without_extras |> Result.map (\i -> i.extras) |> Expect.equal (Ok [])
        , test "Json extras can be passed as a dictionary" <|
            \_ -> raw_decode valid_dict_extras |> Result.map (\i -> i.extras) |> Expect.equal (Ok [ ( "A", [] ), ( "B", [] ) ])
        ]


valid_without_image : String
valid_without_image =
    """{ "expected": [], "actual": [], "diff": [], "extras": [] }"""


nominal : String
nominal =
    """{ 
    "image": { "url": "http://some/url", "width": 1, "height": 1, "refX": 0.0, "refY": 0.0, "pixelWidth": 1.5, "pixelHeight": 1.5 }
    , "expected": [[[0.1, 0.2]]]
    , "actual": [ [[0.3, 0.4], [0.3, 0.4]], [[0.5, 0.6]] ]
    , "diff": [ [[0.7, 0.8], [0.9, 1.0]], [] ]
    , "extras": [
        ["A", [[[1.1, 1.2]]] ]
        , ["B", []]
        ]
    }"""


valid_with_unknown_fields : String
valid_with_unknown_fields =
    """{ 
    "image": { "url": "http://some/url", "width": 1, "height": 1, "refX": 0.0, "refY": 0.0, "pixelWidth": 1.5, "pixelHeight": 1.5 }
    , "expected": [[[0.1, 0.2]]]
    , "actual": [ [[0.3, 0.4], [0.3, 0.4]], [[0.5, 0.6]] ]
    , "diff": [ [[0.7, 0.8], [0.9, 1.0]], [] ]
    , "extras": [
        ["A", [[[1.1, 1.2]]] ]
        , ["B", []]
        ]
    , "definitely-not-a-recognized-field": { "metadata": "gibberish", "maybe-used-by-another-tool": true }
    }"""


decoded_nominal : Init
decoded_nominal =
    { image = Just { url = "http://some/url", width = 1, height = 1, refX = 0.0, refY = 0.0, pixelWidth = 1.5, pixelHeight = 1.5 }
    , expected = [ [ ( 0.1, 0.2 ) ] ]
    , actual = [ [ ( 0.3, 0.4 ), ( 0.3, 0.4 ) ], [ ( 0.5, 0.6 ) ] ]
    , diff = [ [ ( 0.7, 0.8 ), ( 0.9, 1.0 ) ], [] ]
    , extras =
        [ ( "A", [ [ ( 1.1, 1.2 ) ] ] )
        , ( "B", [] )
        ]
    }


valid_without_extras : String
valid_without_extras =
    """{ 
    "image": { "url": "http://some/url", "width": 1, "height": 1, "refX": 0.0, "refY": 0.0, "pixelWidth": 1.5, "pixelHeight": 1.5 }
    , "expected": [], "actual": [], "diff": []
    }"""


valid_dict_extras : String
valid_dict_extras =
    """{ 
    "image": { "url": "http://some/url", "width": 1, "height": 1, "refX": 0.0, "refY": 0.0, "pixelWidth": 1.5, "pixelHeight": 1.5 }
    , "expected": [], "actual": [], "diff": [], "extras": { "A": [], "B": [] }
    }"""


identity_modulo_ok : (a -> Result error a) -> a -> Expect.Expectation
identity_modulo_ok f a =
    Expect.equal (Ok a) (f a)


raw_decode : String -> Result Json.Decode.Error Init
raw_decode s =
    Json.Decode.decodeString decode s


encode_then_decode : Init -> Result Json.Decode.Error Init
encode_then_decode i =
    Json.Decode.decodeValue decode (encode i)


forall_image_spec : Fuzz.Fuzzer ImageSpec
forall_image_spec =
    Fuzz.map7 ImageSpec
        (Fuzz.asciiStringOfLengthBetween 1 100)
        (Fuzz.uniformInt 1)
        (Fuzz.uniformInt 1)
        Fuzz.niceFloat
        Fuzz.niceFloat
        Fuzz.niceFloat
        Fuzz.niceFloat


forall_init : Fuzz.Fuzzer Init
forall_init =
    Fuzz.map5 Init
        (Fuzz.maybe forall_image_spec)
        forall_contour
        forall_contour
        forall_contour
        (Fuzz.listOfLengthBetween 1 10 forall_named_contour)


forall_named_contour : Fuzz.Fuzzer ( String, List (List ( Float, Float )) )
forall_named_contour =
    Fuzz.map2 (\s c -> ( s, c ))
        (Fuzz.stringOfLengthBetween 1 10)
        forall_contour


forall_contour : Fuzz.Fuzzer (List (List ( Float, Float )))
forall_contour =
    Fuzz.listOfLengthBetween 1 10 forall_shape


forall_shape : Fuzz.Fuzzer (List ( Float, Float ))
forall_shape =
    Fuzz.listOfLengthBetween 1 50 forall_points


forall_points : Fuzz.Fuzzer ( Float, Float )
forall_points =
    Fuzz.map2 (\x y -> ( x, y )) Fuzz.niceFloat Fuzz.niceFloat
