module Qol.Cycle exposing (Cycle, associateList, get, length, ofList)

import Array exposing (Array)



-- exposed


type Cycle a
    = Cycle ( a, Array a )


length : Cycle a -> Int
length (Cycle ( _, tail )) =
    1 + Array.length tail


ofList : a -> List a -> Cycle a
ofList head tail =
    Cycle ( head, Array.fromList tail )


get : Int -> Cycle a -> a
get n (Cycle ( head, tail )) =
    let
        cycle =
            Cycle ( head, tail )
    in
    if n <= 0 then
        head

    else
        let
            l =
                length cycle
        in
        if n < l then
            Array.get (n - 1) tail |> Maybe.withDefault head

        else
            get (n - l) cycle


associateList : List a -> Cycle b -> List ( a, b )
associateList l cycle =
    List.indexedMap (\i x -> ( x, get i cycle )) l
