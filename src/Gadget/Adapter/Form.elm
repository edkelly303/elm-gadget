module Gadget.Adapter.Form exposing (Model, Msg, init, submit, update, view)

import Dict exposing (Dict)
import Gadget.IR as IR exposing (Type(..), Value(..), VariantValue(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra


type alias Path =
    List Int


type alias Model =
    Dict Path Value


type alias Msg =
    ( Path, Value )


init : IR.Gadget a -> Model
init gadget =
    initHelp Dict.empty [ 0 ] (IR.irType gadget)


initHelp : Model -> Path -> Type -> Model
initHelp dict path irType =
    case irType of
        UnitType m ->
            Dict.insert path UnitValue dict

        BoolType _ ->
            Dict.insert path (BoolValue False) dict

        CharType _ ->
            Dict.insert path (StringValue "") dict

        StringType _ ->
            Dict.insert path (StringValue "") dict

        IntType _ ->
            Dict.insert path (StringValue "") dict

        FloatType _ ->
            Dict.insert path (StringValue "") dict

        CustomType _ _ _ ->
            Debug.todo "branch 'CustomType _ _ _' not implemented"

        RecordType _ namedFieldTypes ->
            List.Extra.indexedFoldl
                (\idx ( name, fieldType ) output ->
                    initHelp output (idx :: path) fieldType
                )
                dict
                namedFieldTypes

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"


update : IR.Gadget a -> Msg -> Model -> Model
update gadget ( path, msgValue ) model =
    Maybe.map2
        (\modelValue irType -> Dict.insert path (updateHelp irType msgValue modelValue) model)
        (Dict.get path model)
        (typeFromPath path (IR.irType gadget))
        |> Maybe.withDefault model


updateHelp : Type -> Value -> Value -> Value
updateHelp irType msgValue modelValue =
    case irType of
        UnitType _ ->
            UnitValue

        BoolType _ ->
            msgValue

        CharType _ ->
            msgValue

        StringType _ ->
            msgValue

        IntType _ ->
            msgValue

        FloatType _ ->
            msgValue

        CustomType _ _ _ ->
            Debug.todo "branch 'CustomType _ _ _' not implemented"

        RecordType _ namedFieldTypes ->
            Debug.todo "branch 'RecordType _ _' not implemented"

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"


view : IR.Gadget a -> Model -> H.Html Msg
view gadget model =
    let
        irType =
            IR.irType gadget
    in
    Dict.map
        (\path modelValue ->
            case typeFromPath path irType of
                Just modelType ->
                    case ( modelType, modelValue ) of
                        ( IntType _, StringValue s ) ->
                            H.input
                                [ HA.type_ "number"
                                , HE.onInput (\newS -> ( path, StringValue newS ))
                                ]
                                [ H.text s ]

                        ( StringType _, StringValue s ) ->
                            H.input
                                [ HA.type_ "text"
                                , HE.onInput (\newS -> ( path, StringValue newS ))
                                ]
                                [ H.text s ]

                        _ ->
                            H.text "error: mismatched modelType and modelValue"

                Nothing ->
                    H.text "error: type not found"
        )
        model
        |> Dict.values
        |> H.div []


submit : IR.Gadget a -> Model -> Result String a
submit gadget model =
    submitHelp [ 0 ] (IR.irType gadget) model
        |> Result.andThen (IR.toOutput gadget)


submitHelp : Path -> Type -> Model -> Result String Value
submitHelp path irType model =
    case irType of
        UnitType _ ->
            Ok UnitValue

        StringType _ ->
            Dict.get path model
                |> Result.fromMaybe "error"

        IntType _ ->
            case Dict.get path model of
                Just (StringValue s) ->
                    case String.toInt s of
                        Just i ->
                            Ok (IntValue i)

                        Nothing ->
                            Err "Not a valid integer"

                _ ->
                    Err "error"

        BoolType _ ->
            Debug.todo "branch 'BoolType _' not implemented"

        CharType _ ->
            Debug.todo "branch 'CharType _' not implemented"

        FloatType _ ->
            Debug.todo "branch 'FloatType _' not implemented"

        CustomType _ _ _ ->
            Debug.todo "branch 'CustomType _ _ _' not implemented"

        RecordType _ namedFieldTypes ->
            List.indexedMap
                (\idx ( name, fieldType ) ->
                    submitHelp (idx :: path) fieldType model
                        |> Result.map (Tuple.pair name)
                )
                namedFieldTypes
                |> Result.Extra.combine
                |> Result.map RecordValue

        ListType _ _ ->
            Debug.todo "branch 'ListType _ _' not implemented"

        LazyType _ _ ->
            Debug.todo "branch 'LazyType _ _' not implemented"

        TupleType _ _ _ ->
            Debug.todo "branch 'TupleType _ _ _' not implemented"

        TripleType _ _ _ _ ->
            Debug.todo "branch 'TripleType _ _ _ _' not implemented"



-- helpers


typeFromPath : Path -> Type -> Maybe Type
typeFromPath soughtPath irType =
    typeFromPathHelp [ 0 ] soughtPath irType


typeFromPathHelp : Path -> Path -> Type -> Maybe Type
typeFromPathHelp thisPath soughtPath irType =
    if thisPath == soughtPath then
        Just irType

    else
        case irType of
            RecordType _ namedFieldTypes ->
                List.Extra.indexedFoldl
                    (\idx ( name, fieldType ) acc ->
                        case acc of
                            Just foundType ->
                                Just foundType

                            Nothing ->
                                typeFromPathHelp (idx :: thisPath) soughtPath fieldType
                    )
                    Nothing
                    namedFieldTypes

            CustomType _ _ _ ->
                Debug.todo "branch 'CustomType _ _ _' not implemented"

            ListType _ _ ->
                Debug.todo "branch 'ListType _ _' not implemented"

            LazyType _ _ ->
                Debug.todo "branch 'LazyType _ _' not implemented"

            TupleType _ _ _ ->
                Debug.todo "branch 'TupleType _ _ _' not implemented"

            TripleType _ _ _ _ ->
                Debug.todo "branch 'TripleType _ _ _ _' not implemented"

            _ ->
                Nothing
