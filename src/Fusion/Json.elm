module Fusion.Json exposing (..)

import Fusion.Types exposing (..)
import Helpers exposing (..)
import Json.Decode as D exposing (Decoder)
import List.Extra as List


unit : Decoder a -> Decoder ()
unit =
    D.map (\_ -> ())


decoderFromMType : Int -> MType -> ( String, Decoder () )
decoderFromMType indent mtype =
    let
        recurse =
            decoderFromMType (indent + 1)

        i =
            String.repeat (indent * tabSize) " "
    in
    case mtype of
        MInt jp ->
            ( "D.int", D.int |> unit )

        MFloat jp ->
            ( "D.float", D.float |> unit )

        MString jp ->
            ( "D.string", D.string |> unit )

        MBool jp ->
            ( "D.bool", D.bool |> unit )

        MList mtype_ jp ->
            let
                ( first, second ) =
                    recurse mtype_
            in
            ( "D.list (" ++ first ++ ")", D.list second |> unit )

        MCustom name tParams params jp ->
            ( "Debug.crash \"unimplemented decoderFromMType: " ++ toString mtype ++ "\"", D.fail "Not implemented" )

        MRecord name tParams fields jp ->
            case name of
                "Unknown" ->
                    let
                        fieldNames =
                            fields |> List.map (\( fieldName, t ) -> fieldName)

                        anonymousFunction =
                            "(\\" ++ String.join " " fieldNames

                        anonymousRecord =
                            fieldNames
                                |> List.indexedMap
                                    (\j n ->
                                        let
                                            prefix =
                                                if j == 0 then
                                                    "{"

                                                else
                                                    ","
                                        in
                                        i ++ tab ++ tab ++ prefix ++ " " ++ n ++ " = " ++ n
                                    )
                                |> (\v -> v ++ [ i ++ tab ++ tab ++ "}" ])
                    in
                    -- This record is not yet named, used anonymous record syntax
                    ( ([ "D.succeed"
                       , i ++ tab ++ anonymousFunction ++ " ->"
                       ]
                        ++ anonymousRecord
                        ++ [ i ++ tab ++ ")" ]
                        ++ (fields
                                |> List.map
                                    (\( fieldName, t ) ->
                                        i ++ tab ++ "|> required \"" ++ fieldName ++ "\"" ++ parenthesisIfNeeded indent t
                                    )
                           )
                      )
                        |> String.join "\n"
                    , fields
                        |> List.foldl (\( fieldName, t ) soFar -> soFar |> required fieldName (recurse t |> Tuple.second))
                            (D.succeed ())
                    )

                _ ->
                    ( ([ "D.succeed " ++ name
                       ]
                        ++ (fields |> List.map (\( fieldName, t ) -> i ++ tab ++ "|> required \"" ++ fieldName ++ "\" " ++ Tuple.first (recurse t)))
                      )
                        |> String.join "\n"
                    , D.fail "TODO 2"
                    )

        MParam name ->
            if name == "unknown" then
                ( "(Debug.crash \"unspecified type\")", D.fail "Unimplemented" )

            else
                ( "Debug.crash \"unimplemented decoderFromMType: " ++ toString mtype ++ "\"", D.fail "Unimplemented" )

        MMaybe mtype_ jp ->
            let
                ( first, second ) =
                    recurse mtype_
            in
            ( "(D.nullable " ++ first ++ ")", D.nullable second |> unit )

        MRecursive name ->
            ( "Debug.crash \"unimplemented decoderFromMType: " ++ toString mtype ++ "\"", D.fail "Unimplemented" )

        MUnimplemented ->
            ( "Debug.crash \"unimplemented decoderFromMType: " ++ toString mtype ++ "\"", D.fail "Unimplemented" )


parenthesisIfNeeded indent mtype =
    let
        needed =
            case mtype of
                MRecord name tParams fields jp ->
                    True

                _ ->
                    False

        fn =
            if needed then
                decoderFromMType (indent + 2) >> Tuple.first

            else
                decoderFromMType (indent + 1) >> Tuple.first

        i =
            String.repeat (indent * tabSize) " "
    in
    if needed then
        "\n" ++ i ++ tab ++ tab ++ "(" ++ fn mtype ++ "\n" ++ i ++ tab ++ tab ++ ")"

    else
        " " ++ fn mtype


tab =
    String.repeat tabSize " "


tabSize =
    4


required : String -> Decoder () -> Decoder () -> Decoder ()
required key valDecoder decoder =
    D.map2 (\_ _ -> ()) (D.field key valDecoder) decoder


getValue : JsonPath -> JsonValue -> Maybe JsonValue
getValue jp jv =
    case jp of
        Root ->
            Just jv

        At parents field ->
            case jv of
                JObject fields ->
                    case parents of
                        [] ->
                            fields
                                |> List.find (\( n, v ) -> n == field)
                                |> Maybe.map Tuple.second

                        p :: ps ->
                            fields
                                |> List.find (\( n, v ) -> n == p)
                                |> Maybe.andThen
                                    (\( n, v ) ->
                                        getValue (At ps field) v
                                    )

                _ ->
                    Nothing


onRecordField fieldName fn fields =
    case List.find (\( f, t ) -> f == fieldName) fields of
        Just _ ->
            fields
                |> List.updateIf (\( f, t ) -> f == fieldName) fn

        Nothing ->
            List.append fields [ fn ( fieldName, MRecord "Unknown" [] [] Root ) ]


jsonValueToString jv =
    case jv of
        JInt i ->
            "JInt:" ++ String.fromInt i

        JFloat f ->
            "JFloat:" ++ String.fromFloat f

        JString string ->
            "JString:" ++ string

        JBool bool ->
            "JBool:"
                ++ (if bool then
                        "True"

                    else
                        "False"
                   )

        JNull ->
            "JNull"

        JList values ->
            "JList:"
                ++ (case values of
                        [] ->
                            "[]"

                        x :: xs ->
                            jsonValueToString x
                   )

        JObject fields ->
            "JObject"
