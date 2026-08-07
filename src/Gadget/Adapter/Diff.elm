module Gadget.Adapter.Diff exposing (Changes, diff, patch)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

**Warning:** the functions in this module may not do what you expect!

The aim of this module is not to create pretty, human-readable diffs for Elm
values. It's designed to make (fairly) minimal diffs that can be sent over the
wire between two Elm applications to keep data in sync (for example, in a
Lamdera application).

If anyone would like to contribute an example of using Gadgets to create nice
human-readable diffs, I would be happy to add it to this package. I think it is
probably possible with judicious (ab)use of [`Gadget.IR.Metadata`](Gadget-IR#Metadata).


## Example

    import Gadget
    import Gadget.Adapter.Diff

    gadget =
        Gadget.list Gadget.int

    oldValue =
        [ 1, 2, 3 ]

    newValue =
        [ 1, 2, 3, 4 ]

    changes =
        Gadget.Adapter.Diff.diff gadget oldValue newValue

    changes --: Gadget.Adapter.Diff.Changes

    Gadget.Adapter.Diff.patch gadget changes oldValue

    --> Ok [ 1, 2, 3, 4 ]


## API

@docs Changes, diff, patch

-}

import Dict
import Diff as ListDiffer
import Gadget.IR as IR
import List.Extra
import Maybe.Extra
import Result.Extra


type alias IRValue =
    IR.IR IR.Value


type alias IRType =
    IR.IR IR.Type


{-| A set of changes that captures the differences between two Elm values.
-}
type Changes
    = Identical
    | ProductChanges ( Int, Changes ) (List ( Int, Changes ))
    | CustomChanges Int (List ( Int, Changes ))
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
    | ListChanges (List ListChange)
    | TupleChange Changes Changes
    | TripleChange Changes Changes Changes


type ListChange
    = Added Changes
    | Moved Int
    | Updated Int Changes
    | RangeForward Int Int
    | RangeBackward Int Int
    | Repeat Int ListChange


{-| Compare two values and generate [Changes](#Changes).
-}
diff : IR.Gadget a -> a -> a -> Changes
diff gadget old new =
    let
        oldIR =
            IR.fromInput gadget old

        newIR =
            IR.fromInput gadget new

        irType =
            IR.irType gadget
    in
    diffHelp irType oldIR newIR


diffHelp : IRType -> IRValue -> IRValue -> Changes
diffHelp (IR.IR _ type_) ((IR.IR _ oldData) as oldIR_) ((IR.IR _ newData) as newIR_) =
    case type_ of
        IR.LazyType toInnerType ->
            diffHelp (toInnerType ()) oldIR_ newIR_

        _ ->
            if oldData == newData then
                Identical

            else
                case ( oldData, newData, type_ ) of
                    ( IR.BoolValue _, IR.BoolValue b2, _ ) ->
                        BoolChange b2

                    ( IR.StringValue _, IR.StringValue b2, _ ) ->
                        StringChange b2

                    ( IR.CharValue _, IR.CharValue b2, _ ) ->
                        CharChange b2

                    ( IR.FloatValue _, IR.FloatValue b2, _ ) ->
                        FloatChange b2

                    ( IR.IntValue _, IR.IntValue b2, _ ) ->
                        IntChange b2

                    ( IR.ListValue oldList, IR.ListValue newList, IR.ListType itemType ) ->
                        ListDiffer.diffWith (areSimilar itemType) oldList newList
                            |> List.foldl
                                (\change { idx, out } ->
                                    case change of
                                        ListDiffer.Added newItem ->
                                            { idx = idx
                                            , out =
                                                case List.Extra.elemIndex newItem oldList of
                                                    Just oldIdx ->
                                                        Just (Moved oldIdx) :: out

                                                    Nothing ->
                                                        Just (Added (diffHelp itemType (default itemType) newItem)) :: out
                                            }

                                        ListDiffer.Removed _ ->
                                            { idx = idx + 1
                                            , out = Nothing :: out
                                            }

                                        ListDiffer.Similar _ _ changes_ ->
                                            { idx = idx + 1
                                            , out = Just (Updated idx changes_) :: out
                                            }

                                        ListDiffer.NoChange _ ->
                                            { idx = idx + 1
                                            , out = Just (Moved idx) :: out
                                            }
                                )
                                { idx = 0, out = [] }
                            |> .out
                            |> List.filterMap identity
                            |> coalesceForwardMoveSequences
                            |> coalesceBackwardMoveSequences
                            |> doRunLengthEncoding
                            |> ListChanges

                    ( IR.TupleValue a1 b1, IR.TupleValue a2 b2, IR.TupleType aType bType ) ->
                        Identical
                            |> Debug.log "implement diffHelp for Tuple"

                    ( IR.TripleValue a1 b1 c1, IR.TripleValue a2 b2 c2, IR.TripleType aType bType cType ) ->
                        Identical
                            |> Debug.log "implement diffHelp for Triple"

                    ( IR.ProductValue fields1, IR.ProductValue fields2, IR.ProductType fieldTypes ) ->
                        let
                            changes =
                                List.map3 diffHelp
                                    (List.map Tuple.second fieldTypes)
                                    (List.map Tuple.second fields1)
                                    (List.map Tuple.second fields2)
                                    |> List.indexedMap Tuple.pair
                                    |> List.filter (\( _, arg ) -> arg /= Identical)
                        in
                        case changes of
                            change :: restChanges ->
                                ProductChanges change restChanges

                            [] ->
                                Identical

                    ( IR.CustomValue oldSelected ( _, oldVariant ), IR.CustomValue newSelected ( _, newVariant ), IR.CustomType firstVariantType restVariantTypes ) ->
                        let
                            argsToList variant =
                                case variant of
                                    IR.Variant0Value ->
                                        []

                                    IR.Variant1Value a ->
                                        [ a ]

                                    IR.Variant2Value a1 a2 ->
                                        [ a1, a2 ]

                                    IR.Variant3Value a1 a2 a3 ->
                                        [ a1, a2, a3 ]

                                    IR.Variant4Value a1 a2 a3 a4 ->
                                        [ a1, a2, a3, a4 ]

                                    IR.Variant5Value a1 a2 a3 a4 a5 ->
                                        [ a1, a2, a3, a4, a5 ]

                            argTypesToList variantType =
                                case variantType of
                                    IR.Variant0Type ->
                                        []

                                    IR.Variant1Type a ->
                                        [ a ]

                                    IR.Variant2Type a1 a2 ->
                                        [ a1, a2 ]

                                    IR.Variant3Type a1 a2 a3 ->
                                        [ a1, a2, a3 ]

                                    IR.Variant4Type a1 a2 a3 a4 ->
                                        [ a1, a2, a3, a4 ]

                                    IR.Variant5Type a1 a2 a3 a4 a5 ->
                                        [ a1, a2, a3, a4, a5 ]

                            newArgs =
                                argsToList newVariant

                            newArgTypes =
                                List.Extra.getAt newSelected (firstVariantType :: restVariantTypes)
                                    |> Maybe.withDefault firstVariantType
                                    |> Tuple.second
                                    |> argTypesToList

                            diffedArgs =
                                if oldSelected == newSelected then
                                    let
                                        oldArgs =
                                            argsToList oldVariant
                                    in
                                    List.Extra.zip3 oldArgs newArgs newArgTypes
                                        |> List.indexedMap
                                            (\idx ( oldArg, newArg, argType ) ->
                                                ( idx, diffHelp argType oldArg newArg )
                                            )

                                else
                                    List.Extra.zip newArgs newArgTypes
                                        |> List.indexedMap
                                            (\idx ( newArg, argType ) ->
                                                ( idx, diffHelp argType (default argType) newArg )
                                            )
                        in
                        diffedArgs
                            |> List.filter (\( _, arg ) -> arg /= Identical)
                            |> CustomChanges newSelected

                    _ ->
                        Identical


coalesceForwardMoveSequences : List ListChange -> List ListChange
coalesceForwardMoveSequences list =
    List.foldr
        (\item prev ->
            case ( prev, item ) of
                ( [], _ ) ->
                    [ item ]

                ( (Moved prevMove) :: restPrevItems, Moved move ) ->
                    if move == prevMove + 1 then
                        RangeForward prevMove move :: restPrevItems

                    else
                        item :: prev

                ( (RangeForward start end) :: restPrevItems, Moved move ) ->
                    if move == end + 1 then
                        RangeForward start move :: restPrevItems

                    else
                        item :: prev

                _ ->
                    item :: prev
        )
        []
        list


coalesceBackwardMoveSequences : List ListChange -> List ListChange
coalesceBackwardMoveSequences list =
    List.foldl
        (\item prev ->
            case ( prev, item ) of
                ( [], _ ) ->
                    [ item ]

                ( (Moved prevMove) :: restPrevItems, Moved move ) ->
                    if move == prevMove + 1 then
                        RangeBackward move prevMove :: restPrevItems

                    else
                        item :: prev

                ( (RangeBackward end start) :: restPrevItems, Moved move ) ->
                    if move == end + 1 then
                        RangeBackward move start :: restPrevItems

                    else
                        item :: prev

                _ ->
                    item :: prev
        )
        []
        list
        |> List.reverse


doRunLengthEncoding : List ListChange -> List ListChange
doRunLengthEncoding list =
    List.foldr
        (\item prev ->
            case prev of
                [] ->
                    [ item ]

                prevItem :: restPrevItems ->
                    case prevItem of
                        Repeat length runItem ->
                            if item == runItem then
                                Repeat (length + 1) runItem :: restPrevItems

                            else
                                item :: prev

                        _ ->
                            if item == prevItem then
                                Repeat 2 item :: restPrevItems

                            else
                                item :: prev
        )
        []
        list


areSimilar : IRType -> IRValue -> IRValue -> Maybe Changes
areSimilar irType old new =
    let
        oldNewDiff =
            diffHelp irType old new

        defaultNewDiff =
            diffHelp irType (default irType) new
    in
    if size oldNewDiff < size defaultNewDiff then
        Just oldNewDiff

    else
        Nothing


size : Changes -> Int
size changes =
    case changes of
        Identical ->
            0

        ProductChanges c cs ->
            List.map (\( _, x ) -> size x) (c :: cs)
                |> List.sum

        CustomChanges _ cs ->
            List.map (\( _, x ) -> size x) cs
                |> List.sum

        ListChanges cs ->
            List.map
                (\change ->
                    case change of
                        Added addedC ->
                            size addedC

                        Moved _ ->
                            1

                        Updated _ updatedC ->
                            size updatedC

                        RangeForward _ _ ->
                            1

                        RangeBackward _ _ ->
                            1

                        Repeat _ _ ->
                            1
                )
                cs
                |> List.sum

        BoolChange _ ->
            1

        IntChange _ ->
            1

        FloatChange _ ->
            1

        CharChange _ ->
            1

        StringChange _ ->
            1

        TupleChange a b ->
            size a + size b

        TripleChange a b c ->
            size a + size b + size c


default : IRType -> IRValue
default (IR.IR metadata irType) =
    case irType of
        IR.LazyType construct ->
            default (construct ())

        IR.BoolType ->
            IR.IR metadata <| IR.BoolValue True

        IR.CharType ->
            IR.IR metadata <| IR.CharValue ' '

        IR.StringType ->
            IR.IR metadata <| IR.StringValue ""

        IR.IntType ->
            IR.IR metadata <| IR.IntValue 0

        IR.FloatType ->
            IR.IR metadata <| IR.FloatValue 0.0

        IR.ListType _ ->
            IR.IR metadata <| IR.ListValue []

        IR.CustomType ( name, firstVariantType ) _ ->
            IR.IR metadata <|
                IR.CustomValue 0
                    ( name
                    , case firstVariantType of
                        IR.Variant0Type ->
                            IR.Variant0Value

                        IR.Variant1Type arg ->
                            IR.Variant1Value (default arg)

                        IR.Variant2Type arg1 arg2 ->
                            IR.Variant2Value (default arg1) (default arg2)

                        IR.Variant3Type arg1 arg2 arg3 ->
                            IR.Variant3Value (default arg1) (default arg2) (default arg3)

                        IR.Variant4Type arg1 arg2 arg3 arg4 ->
                            IR.Variant4Value (default arg1) (default arg2) (default arg3) (default arg4)

                        IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                            IR.Variant5Value (default arg1) (default arg2) (default arg3) (default arg4) (default arg5)
                    )

        IR.ProductType fieldTypes ->
            IR.IR metadata <| IR.ProductValue (List.map (Tuple.mapSecond default) fieldTypes)

        IR.TupleType aType bType ->
            IR.IR metadata <| IR.TupleValue (default aType) (default bType)

        IR.TripleType aType bType cType ->
            IR.IR metadata <| IR.TripleValue (default aType) (default bType) (default cType)


{-| Use a set of [Changes](#Changes) to patch a value.
-}
patch : IR.Gadget a -> Changes -> a -> Result String a
patch gadget delta old =
    let
        oldIR =
            IR.fromInput gadget old

        irType =
            IR.irType gadget
    in
    case patchHelp delta oldIR irType of
        Ok ir ->
            IR.toOutput gadget ir
                |> Result.mapError (\_ -> "IR.toOutput failed")

        Err e ->
            Err e


patchHelp : Changes -> IRValue -> IRType -> Result String IRValue
patchHelp changes_ ((IR.IR metadata oldData) as old_) (IR.IR _ type_) =
    case ( changes_, oldData, type_ ) of
        ( Identical, _, _ ) ->
            Ok old_

        ( _, _, IR.LazyType constructType ) ->
            patchHelp changes_ old_ (constructType ())

        ( BoolChange b, IR.BoolValue _, _ ) ->
            Ok (IR.IR metadata <| IR.BoolValue b)

        ( CharChange b, IR.CharValue _, _ ) ->
            Ok (IR.IR metadata <| IR.CharValue b)

        ( StringChange b, IR.StringValue _, _ ) ->
            Ok (IR.IR metadata <| IR.StringValue b)

        ( IntChange b, IR.IntValue _, _ ) ->
            Ok (IR.IR metadata <| IR.IntValue b)

        ( FloatChange b, IR.FloatValue _, _ ) ->
            Ok (IR.IR metadata <| IR.FloatValue b)

        ( TupleChange aChange bChange, IR.TupleValue a b, _ ) ->
            Ok (IR.IR metadata <| IR.TupleValue a b)
                |> Debug.log "implement patchHelp for Tuple"

        ( TripleChange aChange bChange cChange, IR.TripleValue a b c, _ ) ->
            Ok (IR.IR metadata <| IR.TripleValue a b c)
                |> Debug.log "implement patchHelp for Triple"

        ( ListChanges cs, IR.ListValue oldList, IR.ListType itemType ) ->
            Ok
                (List.foldl
                    (\change out ->
                        listPatchHelp change oldList itemType :: out
                    )
                    []
                    cs
                    |> List.filterMap identity
                    |> List.concat
                    |> IR.ListValue
                    |> IR.IR metadata
                )

        ( ProductChanges fieldChange restFieldChanges, IR.ProductValue oldFields, IR.ProductType fieldTypes ) ->
            let
                fieldChangesDict =
                    Dict.fromList (fieldChange :: restFieldChanges)
            in
            List.Extra.zip
                oldFields
                fieldTypes
                |> List.indexedMap
                    (\idx ( ( name, oldField ), ( _, fieldType ) ) ->
                        case Dict.get idx fieldChangesDict of
                            Nothing ->
                                Ok ( name, oldField )

                            Just change ->
                                patchHelp change oldField fieldType
                                    |> Result.map (Tuple.pair name)
                    )
                |> Result.Extra.combine
                |> Result.map IR.ProductValue
                |> Result.map (IR.IR metadata)

        ( CustomChanges diffSelected diffVariant, IR.CustomValue oldSelected ( _, oldVariant ), IR.CustomType firstVariantType restVariantTypes ) ->
            let
                argsDict =
                    Dict.fromList diffVariant

                toArgDiff idx arg argType =
                    case Dict.get idx argsDict of
                        Nothing ->
                            Ok arg

                        Just changes ->
                            patchHelp changes arg argType

                toArgDiffFromDefault idx argType =
                    case Dict.get idx argsDict of
                        Nothing ->
                            Ok (default argType)

                        Just changes ->
                            patchHelp changes (default argType) argType
            in
            List.Extra.getAt diffSelected (firstVariantType :: restVariantTypes)
                |> Result.fromMaybe "missing"
                |> Result.andThen
                    (\( name, variantType ) ->
                        Result.map (Tuple.pair name) <|
                            case variantType of
                                IR.Variant0Type ->
                                    Ok IR.Variant0Value

                                IR.Variant1Type arg1Type ->
                                    if diffSelected == oldSelected then
                                        case oldVariant of
                                            IR.Variant1Value arg1 ->
                                                let
                                                    arg1Diff =
                                                        toArgDiff 0 arg1 arg1Type
                                                in
                                                Result.map IR.Variant1Value arg1Diff

                                            _ ->
                                                Err ""

                                    else
                                        let
                                            arg1Diff =
                                                toArgDiffFromDefault 0 arg1Type
                                        in
                                        Result.map IR.Variant1Value arg1Diff

                                IR.Variant2Type arg1Type arg2Type ->
                                    if diffSelected == oldSelected then
                                        case oldVariant of
                                            IR.Variant2Value arg1 arg2 ->
                                                let
                                                    arg1Diff =
                                                        toArgDiff 0 arg1 arg1Type

                                                    arg2Diff =
                                                        toArgDiff 1 arg2 arg2Type
                                                in
                                                Result.map2 IR.Variant2Value arg1Diff arg2Diff

                                            _ ->
                                                Err ""

                                    else
                                        let
                                            arg1Diff =
                                                toArgDiffFromDefault 0 arg1Type

                                            arg2Diff =
                                                toArgDiffFromDefault 1 arg2Type
                                        in
                                        Result.map2 IR.Variant2Value arg1Diff arg2Diff

                                IR.Variant3Type arg1Type arg2Type arg3Type ->
                                    if diffSelected == oldSelected then
                                        case oldVariant of
                                            IR.Variant3Value arg1 arg2 arg3 ->
                                                let
                                                    arg1Diff =
                                                        toArgDiff 0 arg1 arg1Type

                                                    arg2Diff =
                                                        toArgDiff 1 arg2 arg2Type

                                                    arg3Diff =
                                                        toArgDiff 2 arg3 arg3Type
                                                in
                                                Result.map3 IR.Variant3Value arg1Diff arg2Diff arg3Diff

                                            _ ->
                                                Err ""

                                    else
                                        let
                                            arg1Diff =
                                                toArgDiffFromDefault 0 arg1Type

                                            arg2Diff =
                                                toArgDiffFromDefault 1 arg2Type

                                            arg3Diff =
                                                toArgDiffFromDefault 2 arg3Type
                                        in
                                        Result.map3 IR.Variant3Value arg1Diff arg2Diff arg3Diff

                                IR.Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                                    if diffSelected == oldSelected then
                                        case oldVariant of
                                            IR.Variant4Value arg1 arg2 arg3 arg4 ->
                                                let
                                                    arg1Diff =
                                                        toArgDiff 0 arg1 arg1Type

                                                    arg2Diff =
                                                        toArgDiff 1 arg2 arg2Type

                                                    arg3Diff =
                                                        toArgDiff 2 arg3 arg3Type

                                                    arg4Diff =
                                                        toArgDiff 3 arg4 arg4Type
                                                in
                                                Result.map4 IR.Variant4Value arg1Diff arg2Diff arg3Diff arg4Diff

                                            _ ->
                                                Err ""

                                    else
                                        let
                                            arg1Diff =
                                                toArgDiffFromDefault 0 arg1Type

                                            arg2Diff =
                                                toArgDiffFromDefault 1 arg2Type

                                            arg3Diff =
                                                toArgDiffFromDefault 2 arg3Type

                                            arg4Diff =
                                                toArgDiffFromDefault 3 arg4Type
                                        in
                                        Result.map4 IR.Variant4Value arg1Diff arg2Diff arg3Diff arg4Diff

                                IR.Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                                    if diffSelected == oldSelected then
                                        case oldVariant of
                                            IR.Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                                                let
                                                    arg1Diff =
                                                        toArgDiff 0 arg1 arg1Type

                                                    arg2Diff =
                                                        toArgDiff 1 arg2 arg2Type

                                                    arg3Diff =
                                                        toArgDiff 2 arg3 arg3Type

                                                    arg4Diff =
                                                        toArgDiff 3 arg4 arg4Type

                                                    arg5Diff =
                                                        toArgDiff 4 arg5 arg5Type
                                                in
                                                Result.map5 IR.Variant5Value arg1Diff arg2Diff arg3Diff arg4Diff arg5Diff

                                            _ ->
                                                Err ""

                                    else
                                        let
                                            arg1Diff =
                                                toArgDiffFromDefault 0 arg1Type

                                            arg2Diff =
                                                toArgDiffFromDefault 1 arg2Type

                                            arg3Diff =
                                                toArgDiffFromDefault 2 arg3Type

                                            arg4Diff =
                                                toArgDiffFromDefault 3 arg4Type

                                            arg5Diff =
                                                toArgDiffFromDefault 4 arg5Type
                                        in
                                        Result.map5 IR.Variant5Value arg1Diff arg2Diff arg3Diff arg4Diff arg5Diff
                    )
                |> Result.map (IR.CustomValue diffSelected)
                |> Result.map (IR.IR metadata)

        _ ->
            Err "mismatch between diff and value"


listPatchHelp : ListChange -> List IRValue -> IRType -> Maybe (List IRValue)
listPatchHelp change oldList itemType =
    case change of
        Added itemDiff ->
            patchHelp itemDiff (default itemType) itemType
                |> Result.toMaybe
                |> Maybe.map List.singleton

        Moved idx ->
            List.Extra.getAt idx oldList
                |> Maybe.map List.singleton

        Updated idx itemDiff ->
            let
                oldItem =
                    List.Extra.getAt idx oldList
                        |> Maybe.withDefault (default itemType)
            in
            patchHelp itemDiff oldItem itemType
                |> Result.toMaybe
                |> Maybe.map List.singleton

        RangeForward start end ->
            oldList
                |> List.drop start
                |> List.take (1 + end - start)
                |> Just

        RangeBackward start end ->
            oldList
                |> List.drop end
                |> List.take (1 + start - end)
                |> List.reverse
                |> Just

        Repeat length change_ ->
            listPatchHelp change_ oldList itemType
                |> List.repeat length
                |> Maybe.Extra.combine
                |> Maybe.map List.concat
