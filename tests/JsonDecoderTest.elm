module JsonDecoderTest exposing (all)

import Expect exposing (Expectation)
import Fusion.HTTP
import Fusion.Json
import Fusion.Operation
import Fusion.Types exposing (..)
import Json.Decode
import JsonFuzzer
import String.Extra
import Test exposing (describe, fuzz, test)


all =
    describe "Json Decoders"
        [ test "simple" <|
            \() ->
                let
                    json =
                        """{"count": 123}"""
                in
                expectDecodeSuccess json
        , fuzz (JsonFuzzer.json 4 0) "fuzz" <|
            \( jsonEncodeValue, json ) -> expectDecodeSuccess json
        , test "failing" <| \() -> expectDecodeSuccess failingExample
        ]


expectDecodeSuccess : String -> Expectation
expectDecodeSuccess json =
    let
        --json =
        --    """{"count": 123}"""
        jsonValue : JsonValue
        jsonValue =
            case
                json
                    |> String.Extra.unindent
                    |> String.trim
                    |> Json.Decode.decodeString Fusion.HTTP.decodeJsonAst
            of
                Ok okJson ->
                    okJson

                Err error ->
                    Debug.todo <| "Invalid JSON\n\n" ++ Json.Decode.errorToString error

        decoder =
            case Fusion.Operation.fusionAddAll [] jsonValue EmptyDecoder of
                EmptyDecoder ->
                    --TODO what to do for empty?
                    Json.Decode.succeed ()

                FusionType mType ->
                    mType
                        |> Fusion.Json.decoderFromMType 0
                        |> Tuple.second
    in
    Json.Decode.decodeString decoder json
        |> Expect.equal (Ok ())


failingExample : String
failingExample =
    """{
         "data": {
           "blogPostCollection": {
             "items": {
               "title": "Automate with webhooks",
               "description": "Webhooks notify you, another person or system when resources have changed by calling a given HTTP endpoint.",
               "body": "## What are webhooks? The webhooks"
             }
           }
         }
       }
"""
