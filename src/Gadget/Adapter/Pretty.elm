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
                    P.words
                        [ P.string name
                        , case variantValue of
                            Variant0Value ->
                                P.empty

                            Variant1Value arg1 ->
                                P.nest 4 (printHelp arg1)

                            Variant2Value arg1 arg2 ->
                                P.nest 4
                                    (P.words
                                        [ printHelp arg1
                                        , printHelp arg2
                                        ]
                                    )

                            Variant3Value _ _ _ ->
                                Debug.todo "branch 'Variant3Value _ _ _' not implemented"

                            Variant4Value _ _ _ _ ->
                                Debug.todo "branch 'Variant4Value _ _ _ _' not implemented"

                            Variant5Value _ _ _ _ _ ->
                                Debug.todo "branch 'Variant5Value _ _ _ _ _' not implemented"
                        ]

                _ ->
                    P.empty

        ProductValue fields ->
            P.words
                [ P.string "{"
                , P.separators ", "
                    (List.indexedMap
                        (\idx field ->
                            case tools.get (String.fromInt idx) metadata of
                                Just (StringValue name) ->
                                    P.nest 4
                                        (P.words
                                            [ P.string (name ++ " =")
                                            , printHelp field
                                            ]
                                        )

                                _ ->
                                    P.nest 4
                                        (P.words
                                            [ P.string ("#" ++ String.fromInt idx ++ " =")
                                            , printHelp field
                                            ]
                                        )
                        )
                        fields
                        ++ [ P.string "}" ]
                    )
                ]

        ListValue items ->
            P.string "[ "
                |> P.a (P.nest 4 (P.separators ", " (List.map printHelp items)))
                |> P.a (P.string " ]")


bracket : String -> String
bracket s =
    if
        String.contains " " s
            && not (String.startsWith "{" s)
            && not (String.startsWith "(" s)
            && not (String.startsWith "[" s)
    then
        "(" ++ s ++ ")"

    else
        s


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
