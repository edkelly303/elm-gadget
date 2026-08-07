module Gadget.Adapter.Pretty exposing (print)

import Gadget.IR as IR exposing (Value(..), VariantValue(..))
import Pretty as P


tools : IR.MetadataTools a
tools =
    IR.makeMetadataTools "Gadget.Named"


print : IR.Gadget a -> Int -> a -> String
print gadget width input =
    IR.fromInput gadget input
        |> printHelp
        |> P.group
        |> P.pretty width


printHelp : IR.IR Value -> P.Doc t
printHelp (IR.IR metadata value) =
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

        CustomValue selected variantValue ->
            case tools.get (String.fromInt selected) metadata of
                Just (StringValue name) ->
                    P.string name
                        |> P.a
                            (P.nest 4
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
                            )

                _ ->
                    P.empty

        ProductValue fields ->
            case fields of
                [] ->
                    P.string "{}"

                _ ->
                    let
                        item starter idx ( name, fld ) =
                            P.string
                                ((if idx == 0 then
                                    starter ++ " "

                                  else
                                    ""
                                 )
                                    ++ name
                                    ++ " ="
                                )
                                |> P.a
                                    (P.nest 4
                                        (P.line
                                            |> P.a (printHelp fld)
                                        )
                                    )
                                |> P.group
                    in
                    P.separators ", "
                        (List.indexedMap (item "{") fields)
                        |> P.a P.line
                        |> P.a (P.string "}")
                        |> P.group

        ListValue items ->
            case items of
                [] ->
                    P.string "[]"

                _ ->
                    let
                        item starter idx fld =
                            P.string
                                (if idx == 0 then
                                    starter ++ " "

                                 else
                                    ""
                                )
                                |> P.a (printHelp fld)
                                |> P.group
                    in
                    P.separators ", "
                        (List.indexedMap (item "[") items)
                        |> P.a P.line
                        |> P.a (P.string "]")
                        |> P.group


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
