module Fusion.JsonTest exposing (..)

import Dict
import Element exposing (..)
import Element.Font as Font
import Expect
import Fusion.HTTP exposing (..)
import Fusion.Json
import Fusion.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Stubs.MType exposing (..)
import Stubs.Response
import Test exposing (Test, describe, test)
import Types exposing (..)


view =
    let
        expected =
            basic2LevelRecordDecoder

        result =
            Fusion.Json.decoderFromMType 0 basic2LevelRecord
    in
    row [ width fill, Font.family [ Font.monospace ] ]
        [ el [ width fill, alignTop ] <| text expected
        , el [ width fill, alignTop ] <| text (Tuple.first result)
        ]


suite : Test
suite =
    describe "decoderFromTType"
        [ test "2 level simple record field" <|
            \() ->
                let
                    result =
                        Fusion.Json.decoderFromMType 0 basic2LevelRecord

                    expected =
                        basic2LevelRecordDecoder

                    expectUnlines f s =
                        String.lines f |> Expect.equal (String.lines s)
                in
                result
                    |> Tuple.first
                    |> expectUnlines expected
        ]


format =
    D.succeed
        (\first last favorite address ->
            { first = first
            , last = last
            , favorite = favorite
            , address = address
            }
        )
        |> required "first" D.string
        |> required "last" D.string
        |> required "favorite" D.string
        |> required "address"
            (D.succeed
                (\line1 line2 state country ->
                    { line1 = line1
                    , line2 = line2
                    , state = state
                    , country = country
                    }
                )
                |> required "line1" D.string
                |> required "line2" D.string
                |> required "state" D.string
                |> required "country" D.string
            )
