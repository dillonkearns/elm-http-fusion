module JsonFuzzer exposing (json)

-- source: https://github.com/zwilias/elm-json-in-elm/blob/dfda64af641f66330c83e88bf8c0cd28492fc0dc/tests/JsonFuzzer.elm

import Bitwise
import Char
import Fuzz exposing (Fuzzer)
import Json.Decode as Json
import Json.Encode as Encoder


json : Int -> Int -> Fuzzer ( Json.Value, String )
json maxDepth indent =
    rawJson maxDepth
        |> Fuzz.map
            (\value -> ( value, value |> Encoder.encode indent ))


rawJson : Int -> Fuzzer Json.Value
rawJson maxDepth =
    if maxDepth == 0 then
        Fuzz.oneOf <| List.map Tuple.second leaves

    else
        Fuzz.frequency <| leaves ++ (branches <| maxDepth - 1)


leaves : List ( Float, Fuzzer Json.Value )
leaves =
    [ ( 1, Fuzz.constant Encoder.null )
    , ( 3, Fuzz.map Encoder.string hardcoreString )
    , ( 3, Fuzz.map Encoder.int Fuzz.int )
    , ( 3, Fuzz.map Encoder.float Fuzz.float )
    ]


branches : Int -> List ( Float, Fuzzer Json.Value )
branches maxDepth =
    [ ( 1, Fuzz.map (Encoder.list identity) (Fuzz.list (rawJson maxDepth)) )
    , ( 1, Fuzz.map Encoder.object (Fuzz.list (objectEntry maxDepth)) )
    ]


objectEntry : Int -> Fuzzer ( String, Json.Value )
objectEntry maxDepth =
    Fuzz.map2 Tuple.pair
        hardcoreString
        (rawJson maxDepth)


{-| Regular `Fuzz.string` is "just" ASCII. We can do better.
-}
hardcoreString : Fuzzer String
hardcoreString =
    Fuzz.list
        (ranges
            [ ( 0x20, 0x7D )

            -- Skipping `DELETE`
            , ( 0x80, 0xD7FF )

            -- Skipping surrogate pairs
            , ( 0xE000, 0xFFFF )

            -- Emoji!
            , ( 0x0001F600, 0x0001F64F )
            ]
            |> Fuzz.map byteToString
        )
        |> Fuzz.map (String.join "")


byteToString : Int -> String
byteToString int =
    if int <= 0x00010000 then
        Char.fromCode int |> String.fromChar

    else
        let
            c =
                int - 0x00010000
        in
        [ Char.fromCode (Bitwise.shiftRightZfBy 10 c |> Bitwise.or 0xD800)
        , Char.fromCode (Bitwise.and 0x03FF c |> Bitwise.or 0xDC00)
        ]
            |> String.fromList


ranges : List ( Int, Int ) -> Fuzzer Int
ranges =
    let
        makeRange : ( Int, Int ) -> ( Float, Fuzzer Int )
        makeRange ( begin, end ) =
            ( toFloat (end - begin), Fuzz.intRange begin end )
    in
    List.map makeRange >> Fuzz.frequency
