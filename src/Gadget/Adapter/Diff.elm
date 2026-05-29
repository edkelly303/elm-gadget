module Gadget.Adapter.Diff exposing (Changes, diff, patch)

{-| **Warning:** the functions in this module may not do what you expect!

The aim of this module is not to create pretty, human-readable diffs for Elm
values. It's designed to make (fairly) minimal diffs that can be sent over the
wire between two Elm applications to keep data in sync (for example, in a
Lamdera application).

If anyone would like to contribute an example of using Gadgets to create nice
human-readable diffs, I would be happy to add it to this package. I think it is
probably possible with judicious (ab)use of Gadget.label.

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

@docs Changes, diff, patch

-}

import Dict
import Diff as ListDiffer
import Gadget.IR as IR
import List.Extra
import Maybe.Extra
import Result.Extra


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
diff codec old new =
    let
        oldIR =
            IR.fromInput codec old

        newIR =
            IR.fromInput codec new

        irType =
            IR.irType codec
    in
    diffHelp irType oldIR newIR


diffHelp : IR.IRType -> IR.IR -> IR.IR -> Changes
diffHelp irType_ oldIR_ newIR_ =
    if oldIR_ == newIR_ then
        Identical

    else
        case ( oldIR_, newIR_, irType_ ) of
            ( IR.Labelled _ inner1, IR.Labelled _ inner2, IR.LabelledType _ innerType ) ->
                diffHelp innerType inner1 inner2

            ( IR.Bool _, IR.Bool b2, _ ) ->
                BoolChange b2

            ( IR.String _, IR.String b2, _ ) ->
                StringChange b2

            ( IR.Char _, IR.Char b2, _ ) ->
                CharChange b2

            ( IR.Float _, IR.Float b2, _ ) ->
                FloatChange b2

            ( IR.Int _, IR.Int b2, _ ) ->
                IntChange b2

            ( IR.List oldList, IR.List newList, IR.ListType itemType ) ->
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

            ( IR.Product fields1, IR.Product fields2, IR.ProductType fieldTypes ) ->
                let
                    changes =
                        List.map3 diffHelp fieldTypes fields1 fields2
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, arg ) -> arg /= Identical)
                in
                case changes of
                    change :: restChanges ->
                        ProductChanges change restChanges

                    [] ->
                        Identical

            ( IR.Custom oldSelected oldVariant, IR.Custom newSelected newVariant, IR.CustomType firstVariantType restVariantTypes ) ->
                let
                    argsToList variant =
                        case variant of
                            IR.Variant0 ->
                                []

                            IR.Variant1 a ->
                                [ a ]

                            IR.Variant2 a1 a2 ->
                                [ a1, a2 ]

                            IR.Variant3 a1 a2 a3 ->
                                [ a1, a2, a3 ]

                            IR.Variant4 a1 a2 a3 a4 ->
                                [ a1, a2, a3, a4 ]

                            IR.Variant5 a1 a2 a3 a4 a5 ->
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


areSimilar : IR.IRType -> IR.IR -> IR.IR -> Maybe Changes
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


default : IR.IRType -> IR.IR
default irType =
    case irType of
        IR.LabelledType label x ->
            IR.Labelled label (default x)

        IR.LazyType construct ->
            default (construct ())

        IR.BoolType ->
            IR.Bool True

        IR.CharType ->
            IR.Char ' '

        IR.StringType ->
            IR.String ""

        IR.IntType ->
            IR.Int 0

        IR.FloatType ->
            IR.Float 0.0

        IR.ListType _ ->
            IR.List []

        IR.CustomType firstVariantType _ ->
            IR.Custom 0
                (case firstVariantType of
                    IR.Variant0Type ->
                        IR.Variant0

                    IR.Variant1Type arg ->
                        IR.Variant1 (default arg)

                    IR.Variant2Type arg1 arg2 ->
                        IR.Variant2 (default arg1) (default arg2)

                    IR.Variant3Type arg1 arg2 arg3 ->
                        IR.Variant3 (default arg1) (default arg2) (default arg3)

                    IR.Variant4Type arg1 arg2 arg3 arg4 ->
                        IR.Variant4 (default arg1) (default arg2) (default arg3) (default arg4)

                    IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                        IR.Variant5 (default arg1) (default arg2) (default arg3) (default arg4) (default arg5)
                )

        IR.ProductType fieldTypes ->
            IR.Product (List.map default fieldTypes)


{-| Use a set of [Changes](#Changes) to patch a value.
-}
patch : IR.Gadget a -> Changes -> a -> Result String a
patch codec delta old =
    let
        oldIR =
            IR.fromInput codec old

        irType =
            IR.irType codec
    in
    case patchHelp delta oldIR irType of
        Ok ir ->
            IR.toOutput codec ir
                |> Result.mapError (\_ -> "IR.toOutput failed")

        Err e ->
            Err e


patchHelp : Changes -> IR.IR -> IR.IRType -> Result String IR.IR
patchHelp changes_ old_ irType_ =
    case ( changes_, old_, irType_ ) of
        ( Identical, _, _ ) ->
            Ok old_

        ( _, _, IR.LazyType constructType ) ->
            patchHelp changes_ old_ (constructType ())

        ( _, IR.Labelled label inner, IR.LabelledType _ innerType ) ->
            patchHelp changes_ inner innerType
                |> Result.map (IR.Labelled label)

        ( BoolChange b, IR.Bool _, _ ) ->
            Ok (IR.Bool b)

        ( CharChange b, IR.Char _, _ ) ->
            Ok (IR.Char b)

        ( StringChange b, IR.String _, _ ) ->
            Ok (IR.String b)

        ( IntChange b, IR.Int _, _ ) ->
            Ok (IR.Int b)

        ( FloatChange b, IR.Float _, _ ) ->
            Ok (IR.Float b)

        ( ListChanges cs, IR.List oldList, IR.ListType itemType ) ->
            Ok
                (List.foldl
                    (\change out ->
                        listPatchHelp change oldList itemType :: out
                    )
                    []
                    cs
                    |> List.filterMap identity
                    |> List.concat
                    |> IR.List
                )

        ( ProductChanges fieldChange restFieldChanges, IR.Product oldFields, IR.ProductType fieldTypes ) ->
            let
                fieldChangesDict =
                    Dict.fromList (fieldChange :: restFieldChanges)
            in
            List.Extra.zip oldFields fieldTypes
                |> List.indexedMap
                    (\idx ( oldField, fieldType ) ->
                        case Dict.get idx fieldChangesDict of
                            Nothing ->
                                Ok oldField

                            Just change ->
                                patchHelp change oldField fieldType
                    )
                |> Result.Extra.combine
                |> Result.map IR.Product

        ( CustomChanges diffSelected diffVariant, IR.Custom oldSelected oldVariant, IR.CustomType firstVariantType restVariantTypes ) ->
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
                |> Result.fromMaybe ""
                |> Result.andThen
                    (\variantType ->
                        case variantType of
                            IR.Variant0Type ->
                                Ok IR.Variant0

                            IR.Variant1Type arg1Type ->
                                if diffSelected == oldSelected then
                                    case oldVariant of
                                        IR.Variant1 arg1 ->
                                            let
                                                arg1Diff =
                                                    toArgDiff 0 arg1 arg1Type
                                            in
                                            Result.map IR.Variant1 arg1Diff

                                        _ ->
                                            Err ""

                                else
                                    let
                                        arg1Diff =
                                            toArgDiffFromDefault 0 arg1Type
                                    in
                                    Result.map IR.Variant1 arg1Diff

                            IR.Variant2Type arg1Type arg2Type ->
                                if diffSelected == oldSelected then
                                    case oldVariant of
                                        IR.Variant2 arg1 arg2 ->
                                            let
                                                arg1Diff =
                                                    toArgDiff 0 arg1 arg1Type

                                                arg2Diff =
                                                    toArgDiff 1 arg2 arg2Type
                                            in
                                            Result.map2 IR.Variant2 arg1Diff arg2Diff

                                        _ ->
                                            Err ""

                                else
                                    let
                                        arg1Diff =
                                            toArgDiffFromDefault 0 arg1Type

                                        arg2Diff =
                                            toArgDiffFromDefault 1 arg2Type
                                    in
                                    Result.map2 IR.Variant2 arg1Diff arg2Diff

                            IR.Variant3Type arg1Type arg2Type arg3Type ->
                                if diffSelected == oldSelected then
                                    case oldVariant of
                                        IR.Variant3 arg1 arg2 arg3 ->
                                            let
                                                arg1Diff =
                                                    toArgDiff 0 arg1 arg1Type

                                                arg2Diff =
                                                    toArgDiff 1 arg2 arg2Type

                                                arg3Diff =
                                                    toArgDiff 2 arg3 arg3Type
                                            in
                                            Result.map3 IR.Variant3 arg1Diff arg2Diff arg3Diff

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
                                    Result.map3 IR.Variant3 arg1Diff arg2Diff arg3Diff

                            IR.Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                                if diffSelected == oldSelected then
                                    case oldVariant of
                                        IR.Variant4 arg1 arg2 arg3 arg4 ->
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
                                            Result.map4 IR.Variant4 arg1Diff arg2Diff arg3Diff arg4Diff

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
                                    Result.map4 IR.Variant4 arg1Diff arg2Diff arg3Diff arg4Diff

                            IR.Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                                if diffSelected == oldSelected then
                                    case oldVariant of
                                        IR.Variant5 arg1 arg2 arg3 arg4 arg5 ->
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
                                            Result.map5 IR.Variant5 arg1Diff arg2Diff arg3Diff arg4Diff arg5Diff

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
                                    Result.map5 IR.Variant5 arg1Diff arg2Diff arg3Diff arg4Diff arg5Diff
                    )
                |> Result.map (IR.Custom diffSelected)

        _ ->
            Err "mismatch between diff and value"


listPatchHelp : ListChange -> List IR.IR -> IR.IRType -> Maybe (List IR.IR)
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
