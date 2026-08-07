module Gadget.Adapter.Pretty exposing (print)

import Gadget.IR as IR exposing (Value(..), VariantValue(..))
import Pretty as P


print : IR.Gadget a -> Int -> a -> String
print gadget width input =
    IR.fromInput gadget input
        |> printHelp
        |> P.group
        |> P.pretty width


printHelp : IR.IR Value -> P.Doc t
printHelp (IR.IR _ value) =
    case value of
        StringValue s ->
            P.string ("\"" ++ escape s ++ "\"")

        BoolValue b ->
            P.string
                (if b then
                    "True"

                 else
                    "False"
                )

        CharValue c ->
            P.string ("'" ++ String.fromChar c ++ "'")

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
                                    printHelp arg1

                                Variant2Value arg1 arg2 ->
                                    printHelp arg1
                                        |> P.a P.line
                                        |> P.a (printHelp arg2)

                                Variant3Value _ _ _ ->
                                    Debug.todo "branch 'Variant3Value _ _ _' not implemented"

                                Variant4Value _ _ _ _ ->
                                    Debug.todo "branch 'Variant4Value _ _ _ _' not implemented"

                                Variant5Value _ _ _ _ _ ->
                                    Debug.todo "branch 'Variant5Value _ _ _ _ _' not implemented"
                            )
                    )
                |> P.hang 2

        ProductValue fields ->
            case fields of
                [] ->
                    P.string "{}"

                _ ->
                    let
                        printField ( name, fld ) =
                            P.string (name ++ " =")
                                |> P.a P.line
                                |> P.nest 4
                                |> P.a (printHelp fld)
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
                        (P.separators ", " (List.map printHelp items))
                        |> P.align

        TupleValue a b ->
            P.surround
                (P.string "( ")
                (P.line |> P.a (P.string ")"))
                (P.separators ", " [ printHelp a, printHelp b ])
                |> P.align

        TripleValue a b c ->
            P.surround
                (P.string "( ")
                (P.line |> P.a (P.string ")"))
                (P.separators ", " [ printHelp a, printHelp b, printHelp c ])
                |> P.align


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
