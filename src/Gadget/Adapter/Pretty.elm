module Gadget.Adapter.Pretty exposing (print)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to convert Elm values into a pretty-printed `String`, using
[`the-sett/elm-pretty-printer`](https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/).


## API

@docs print

-}

import Gadget.IR as IR exposing (Value(..), VariantValue(..))
import Pretty as P


{-| Print an Elm value as a `String`, wrapped prettily at a given number of
columns
-}
print : IR.Gadget a -> Int -> a -> String
print gadget width input =
    IR.fromInput gadget input
        |> printHelp False
        |> P.group
        |> P.pretty width


printHelp : Bool -> IR.IR Value -> P.Doc t
printHelp parentIsCustom (IR.IR _ value) =
    case value of
        UnitValue ->
            P.string "()"

        BoolValue b ->
            P.string
                (if b then
                    "True"

                 else
                    "False"
                )

        CharValue c ->
            P.string ("'" ++ String.fromChar c ++ "'")

        StringValue s ->
            P.string ("\"" ++ escape s ++ "\"")

        IntValue i ->
            P.string (String.fromInt i)

        FloatValue f ->
            P.string (String.fromFloat f)

        CustomValue _ ( name, variantValue ) ->
            P.string name
                |> P.a
                    (P.line
                        |> P.a
                            (case variantValue of
                                Variant0Value ->
                                    P.empty

                                Variant1Value arg1 ->
                                    printHelp True arg1

                                Variant2Value arg1 arg2 ->
                                    printHelp True arg1
                                        |> P.a P.line
                                        |> P.a (printHelp True arg2)

                                Variant3Value _ _ _ ->
                                    Debug.todo "branch 'Variant3Value _ _ _' not implemented"

                                Variant4Value _ _ _ _ ->
                                    Debug.todo "branch 'Variant4Value _ _ _ _' not implemented"

                                Variant5Value _ _ _ _ _ ->
                                    Debug.todo "branch 'Variant5Value _ _ _ _ _' not implemented"
                            )
                    )
                |> P.group
                |> P.hang 2
                |> P.surround
                    (if parentIsCustom && variantValue /= Variant0Value then
                        P.string "("

                     else
                        P.empty
                    )
                    (if parentIsCustom && variantValue /= Variant0Value then
                        P.tightline |> P.a (P.string ")")

                     else
                        P.empty
                    )

        RecordValue fields ->
            case fields of
                [] ->
                    P.string "{}"

                _ ->
                    let
                        printField ( name, fld ) =
                            P.string (name ++ " =")
                                |> P.a P.line
                                |> P.nest 4
                                |> P.a (printHelp False fld)
                                |> P.group
                    in
                    P.surround
                        (P.string "{ ")
                        (P.line |> P.a (P.string "}"))
                        (P.separators ", " (List.map printField fields))
                        |> P.group
                        |> P.align

        ListValue items ->
            case items of
                [] ->
                    P.string "[]"

                _ ->
                    P.surround
                        (P.string "[ ")
                        (P.line |> P.a (P.string "]"))
                        (P.separators ", " (List.map (printHelp False) items))
                        |> P.align

        TupleValue a b ->
            P.surround
                (P.string "( ")
                (P.line |> P.a (P.string ")"))
                (P.separators ", " [ printHelp False a, printHelp False b ])
                |> P.align

        TripleValue a b c ->
            P.surround
                (P.string "( ")
                (P.line |> P.a (P.string ")"))
                (P.separators ", " [ printHelp False a, printHelp False b, printHelp False c ])
                |> P.align


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
