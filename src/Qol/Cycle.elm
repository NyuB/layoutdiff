module Qol.Cycle exposing (Cycle, associateList, get, length, ofList)

import Array exposing (Array)



-- exposed


{-| Cyclic container with at least one element
-}
type Cycle a
    = Cycle ( a, Array a )


{-| -}
length : Cycle a -> Int
length cycle =
    1 + Array.length (tail cycle)


ofList : a -> List a -> Cycle a
ofList firstElement rest =
    Cycle ( firstElement, Array.fromList rest )


{-| Get element at index in the cycle, modulo the cycle length. A negative index yields the first element.

    cycle = ofList "A" ["B", "C"]

    get 0 cycle == "A"
    get 1 cycle == "B"
    get 2 cycle == "C"
    get 3 cycle == "A"

-}
get : Int -> Cycle a -> a
get index cycle =
    let
        h =
            head cycle
    in
    if index <= 0 then
        h

    else
        let
            l =
                length cycle
        in
        if index < l then
            -- because of the invariants, the withDefault call is just here to satisfy the compiler and will never actually be applied
            let
                t =
                    tail cycle
            in
            Array.get (index - 1) t |> Maybe.withDefault h

        else
            get (index - l) cycle


{-| Associate each element of l with one of cycle, cycling back as needed

    associateList [ 1, 2, 3 ] (ofList "A" [ "B" ]) == [ ( 1, "A" ), ( 2, "B" ), ( 3, "A" ) ]

-}
associateList : List a -> Cycle b -> List ( a, b )
associateList l cycle =
    List.indexedMap (\i x -> ( x, get i cycle )) l



-- internals


head : Cycle a -> a
head (Cycle ( h, _ )) =
    h


tail : Cycle a -> Array a
tail (Cycle ( _, t )) =
    t
