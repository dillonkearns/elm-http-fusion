module Fusion.Transform exposing (..)

import Fusion.Types exposing (..)
import Transform


recurseMType : (MType -> MType) -> MType -> MType
recurseMType fn mtype =
    case mtype of
        MInt jp ->
            MInt jp

        MFloat jp ->
            MFloat jp

        MString jp ->
            MString jp

        MBool jp ->
            MBool jp

        MList mType_ jp ->
            MList (fn mType_) jp

        MCustom name params variants jp ->
            MCustom name (List.map fn params) (List.map (\( l, v ) -> ( l, List.map fn v )) variants) jp

        MRecord name params fields jp ->
            MRecord name (List.map fn params) (List.map (\( l, v ) -> ( l, fn v )) fields) jp

        MParam name ->
            MParam name

        MMaybe mType_ jp ->
            MMaybe (fn mType_) jp

        MRecursive name ->
            MRecursive name

        MUnimplemented ->
            MUnimplemented


recurseChildrenMType : (MType -> List MType) -> MType -> List MType
recurseChildrenMType fn mtype =
    case mtype of
        MInt jp ->
            []

        MFloat jp ->
            []

        MString jp ->
            []

        MBool jp ->
            []

        MList mType_ jp ->
            fn mType_

        MCustom name params variants jp ->
            List.map fn params ++ (List.map (\( l, v ) -> List.map fn v) variants |> List.concat) |> List.concat

        MRecord name params fields jp ->
            List.map fn params ++ List.map (\( l, v ) -> fn v) fields |> List.concat

        MParam name ->
            []

        MMaybe mType_ jp ->
            fn mType_

        MRecursive name ->
            []

        MUnimplemented ->
            []


mapToType : MType -> TType
mapToType mType =
    case mType of
        MInt _ ->
            TInt

        MFloat _ ->
            TFloat

        MString _ ->
            TString

        MBool _ ->
            TBool

        MList mType_ _ ->
            TList (mapToType mType_)

        MCustom name params variants _ ->
            TCustom name (List.map mapToType params) (List.map (\( l, v ) -> ( l, List.map mapToType v )) variants)

        MRecord name params fields _ ->
            TRecord name (List.map mapToType params) (List.map (\( l, v ) -> ( l, mapToType v )) fields)

        MParam name ->
            TParam name

        MMaybe mType_ _ ->
            TMaybe (mapToType mType_)

        MRecursive name ->
            TRecursive name

        MUnimplemented ->
            TUnimplemented


decoderToMType : FusionDecoder -> MType
decoderToMType decoder =
    case decoder of
        EmptyDecoder ->
            MUnimplemented

        FusionType mType ->
            mType
