module Cycle_test exposing (suite)

import Expect exposing (Expectation)
import Qol.Cycle exposing (associateList, get, length, ofList)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cyclic container test"
        [ describe "get" <| quick_tests [ zero_is_head_element, get_mid_element, get_last_element, get_first_element_cyclic, get_mid_element_cyclic ]
        , describe "associate" <| quick_tests [ associate_empty, associate_no_cycle, associate_cycle ]
        , describe "length" <| quick_tests [ length_one, length_many ]
        ]


length_one : Quick_test
length_one =
    ( "Single element cycle length is 1"
    , \_ ->
        Expect.equal 1 (ofList "A" [] |> length)
    )


length_many : Quick_test
length_many =
    ( "3 elements cycle length is 3"
    , \_ ->
        Expect.equal 3 (ofList "A" [ "B", "C" ] |> length)
    )


zero_is_head_element : Quick_test
zero_is_head_element =
    ( "get 0"
    , \_ ->
        let
            cycle =
                ofList "A" []
        in
        Expect.equal "A" (get 0 cycle)
    )


get_mid_element : Quick_test
get_mid_element =
    ( "get middle element"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equal "C" (get 2 cycle)
    )


get_last_element : Quick_test
get_last_element =
    ( "get last element"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equal "D" (get 3 cycle)
    )


get_first_element_cyclic : Quick_test
get_first_element_cyclic =
    ( "get first element by cycling"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equal "A" (get 4 cycle)
    )


get_mid_element_cyclic : Quick_test
get_mid_element_cyclic =
    ( "get mid element by cycling"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equal "B" (get 5 cycle)
    )


associate_empty : Quick_test
associate_empty =
    ( "associate [] _ == []"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equalLists [] (associateList [] cycle)
    )


associate_no_cycle : Quick_test
associate_no_cycle =
    ( "associate when size of list is inferior to cycle"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equalLists [ ( 1, "A" ), ( 2, "B" ), ( 3, "C" ) ] (associateList [ 1, 2, 3 ] cycle)
    )


associate_cycle : Quick_test
associate_cycle =
    ( "associate when size of list is superior to cycle"
    , \_ ->
        let
            cycle =
                ofList "A" [ "B", "C", "D" ]
        in
        Expect.equalLists [ ( 1, "A" ), ( 2, "B" ), ( 3, "C" ) ] (associateList [ 1, 2, 3 ] cycle)
    )


type alias Quick_test =
    ( String, () -> Expectation )


quick_test : ( String, () -> Expectation ) -> Test
quick_test ( name, t ) =
    test name t


quick_tests : List Quick_test -> List Test
quick_tests l =
    List.map quick_test l
