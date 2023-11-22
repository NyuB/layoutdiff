module Init exposing (ImageSpec, Init, InitContour, decode, encode)

import Dict
import Json.Decode as D
import Json.Encode as E



-- exposed


type alias Init =
    { image : Maybe ImageSpec
    , expected : InitContour
    , actual : InitContour
    , diff : InitContour
    , extras : List ( String, InitContour )
    }


type alias ImageSpec =
    { url : String
    , width : Int
    , height : Int
    , refX : Float
    , refY : Float
    , pixelWidth : Float
    , pixelHeight : Float
    }


type alias InitContour =
    List (List ( Float, Float ))


decode : D.Decoder Init
decode =
    D.oneOf [ decode_v1, decode_v2 ]


encode : Init -> E.Value
encode i =
    E.object
        [ ( "image", encode_nullable_image_spec i.image )
        , ( "expected", encode_contour i.expected )
        , ( "actual", encode_contour i.actual )
        , ( "diff", encode_contour i.diff )
        , ( "extras", encode_named_contours i.extras )
        ]



-- internals


type alias NamedContour =
    { name : String
    , shapes : InitContour
    }


decode_v1 : D.Decoder Init
decode_v1 =
    D.map5 Init
        (maybe_nullable "image" decode_image_spec)
        (D.field "expected" decode_contour)
        (D.field "actual" decode_contour)
        (D.field "diff" decode_contour)
        (maybe_nullable "extras" decode_named_contours |> with_default [])


decode_v2 : D.Decoder Init
decode_v2 =
    D.map2
        (\img contours ->
            { image = img
            , expected = find_named_contour_or_empty contours "expected"
            , actual = find_named_contour_or_empty contours "actual"
            , diff = find_named_contour_or_empty contours "diff"
            , extras = without_named_contours contours [ "expected", "actual", "diff" ]
            }
        )
        (maybe_nullable "image" decode_image_spec)
        (D.field "contours" (D.list decode_named_contour_object))


find_named_contour_or_empty : List NamedContour -> String -> InitContour
find_named_contour_or_empty l n =
    List.filterMap
        (\nc ->
            if nc.name == n then
                Just nc.shapes

            else
                Nothing
        )
        l
        |> List.head
        |> Maybe.withDefault []


without_named_contours : List NamedContour -> List String -> List ( String, InitContour )
without_named_contours l names =
    List.filterMap
        (\nc ->
            if List.member nc.name names then
                Nothing

            else
                Just ( nc.name, nc.shapes )
        )
        l


decode_named_contour_object : D.Decoder NamedContour
decode_named_contour_object =
    D.map2 NamedContour
        (D.field "name" D.string)
        (D.field "shapes" decode_contour)


decode_image_spec : D.Decoder ImageSpec
decode_image_spec =
    D.map7 ImageSpec
        (string_field "url")
        (int_field "width")
        (int_field "height")
        (float_field "refX")
        (float_field "refY")
        (float_field "pixelWidth")
        (float_field "pixelHeight")


decode_named_contours : D.Decoder (List ( String, InitContour ))
decode_named_contours =
    D.oneOf [ decode_named_contour_dict, decode_named_contour_list ]


decode_named_contour_list : D.Decoder (List ( String, InitContour ))
decode_named_contour_list =
    D.list decode_named_contour


decode_named_contour_dict : D.Decoder (List ( String, InitContour ))
decode_named_contour_dict =
    D.dict decode_contour |> D.map Dict.toList


decode_named_contour : D.Decoder ( String, InitContour )
decode_named_contour =
    D.map2 (\n c -> ( n, c )) (D.index 0 D.string) (D.index 1 decode_contour)


decode_contour : D.Decoder InitContour
decode_contour =
    D.list decode_shape


decode_shape : D.Decoder (List ( Float, Float ))
decode_shape =
    D.list decode_point


decode_point : D.Decoder ( Float, Float )
decode_point =
    D.map2 (\x y -> ( x, y )) (D.index 0 D.float) (D.index 1 D.float)


encode_contour : InitContour -> E.Value
encode_contour c =
    E.list encode_shape c


encode_shape : List ( Float, Float ) -> E.Value
encode_shape pts =
    E.list encode_point pts


encode_point : ( Float, Float ) -> E.Value
encode_point ( x, y ) =
    E.list E.float [ x, y ]


encode_named_contours : List ( String, InitContour ) -> E.Value
encode_named_contours nc =
    E.list encode_named_contour nc


encode_named_contour : ( String, InitContour ) -> E.Value
encode_named_contour ( name, contour ) =
    E.list
        encode_string_or_contour
        [ S name, C contour ]


{-| Thin wrapper to type heterogeneous arrays
-}
type SC
    = S String
    | C InitContour


encode_string_or_contour : SC -> E.Value
encode_string_or_contour sc =
    case sc of
        S s ->
            E.string s

        C c ->
            encode_contour c


encode_image_spec : ImageSpec -> E.Value
encode_image_spec i =
    E.object
        [ ( "url", E.string i.url )
        , ( "width", E.int i.width )
        , ( "height", E.int i.height )
        , ( "refX", E.float i.refX )
        , ( "refY", E.float i.refY )
        , ( "pixelWidth", E.float i.pixelWidth )
        , ( "pixelHeight", E.float i.pixelHeight )
        ]


encode_nullable_image_spec : Maybe ImageSpec -> E.Value
encode_nullable_image_spec i =
    i |> Maybe.map encode_image_spec |> Maybe.withDefault E.null


int_field : String -> D.Decoder Int
int_field name =
    D.field name D.int


string_field : String -> D.Decoder String
string_field name =
    D.field name D.string


float_field : String -> D.Decoder Float
float_field name =
    D.field name D.float


maybe_nullable : String -> D.Decoder a -> D.Decoder (Maybe a)
maybe_nullable name decoder =
    D.oneOf
        [ D.maybe (D.field name decoder), D.field name (D.nullable decoder) ]


with_default : a -> D.Decoder (Maybe a) -> D.Decoder a
with_default a d =
    D.map (Maybe.withDefault a) d
