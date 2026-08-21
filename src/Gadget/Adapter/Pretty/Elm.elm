module Gadget.Adapter.Pretty.Elm exposing (toDocument)

import Gadget.IR as IR exposing (Value(..), VariantValue(..))
import Lib.Glam as G


toDocument : Value -> G.Document
toDocument =
    toDocumentHelp False


toDocumentHelp : Bool -> IR.Value -> G.Document
toDocumentHelp isChildOfCustomType value =
    case value of
        UnitValue ->
            G.fromString "()"

        BoolValue b ->
            G.fromString
                (if b then
                    "True"

                 else
                    "False"
                )

        CharValue c ->
            G.fromString ("'" ++ String.fromChar c ++ "'")

        StringValue s ->
            G.fromString ("\"" ++ escape s ++ "\"")

        IntValue i ->
            G.fromString (String.fromInt i)

        FloatValue f ->
            G.fromString (String.fromFloat f)

        CustomValue _ ( name, variantValue ) ->
            G.group <|
                (if isChildOfCustomType && variantValue /= Variant0Value then
                    parens

                 else
                    identity
                )
                <|
                    G.nest 4 <|
                        breakable
                            [ G.fromString name
                            , case variantValue of
                                Variant0Value ->
                                    G.empty

                                Variant1Value arg1 ->
                                    toDocumentHelp True arg1

                                Variant2Value arg1 arg2 ->
                                    breakable
                                        [ toDocumentHelp True arg1
                                        , toDocumentHelp True arg2
                                        ]

                                Variant3Value arg1 arg2 arg3 ->
                                    breakable
                                        [ toDocumentHelp True arg1
                                        , toDocumentHelp True arg2
                                        , toDocumentHelp True arg3
                                        ]

                                Variant4Value arg1 arg2 arg3 arg4 ->
                                    breakable
                                        [ toDocumentHelp True arg1
                                        , toDocumentHelp True arg2
                                        , toDocumentHelp True arg3
                                        , toDocumentHelp True arg4
                                        ]

                                Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                                    breakable
                                        [ toDocumentHelp True arg1
                                        , toDocumentHelp True arg2
                                        , toDocumentHelp True arg3
                                        , toDocumentHelp True arg4
                                        , toDocumentHelp True arg5
                                        ]
                            ]

        RecordValue fields ->
            case fields of
                [] ->
                    G.fromString "{}"

                h :: t ->
                    let
                        printField ( name, fld ) =
                            G.group <|
                                G.nest 4 <|
                                    breakable
                                        [ G.fromString (name ++ " =")
                                        , toDocumentHelp False fld
                                        ]
                    in
                    G.group <|
                        breakable
                            (List.concat
                                [ [ unbreakable [ G.fromString "{", printField h ] ]
                                , List.map (\doc -> G.nest 2 <| unbreakable [ G.fromString ",", printField doc ]) t
                                , [ G.fromString "}" ]
                                ]
                            )

        ListValue items ->
            case items of
                [] ->
                    G.fromString "[]"

                h :: t ->
                    G.group <|
                        breakable
                            (List.concat
                                [ [ unbreakable [ G.fromString "[", toDocumentHelp False h ] ]
                                , List.map (\doc -> G.nest 2 <| unbreakable [ G.fromString ",", toDocumentHelp False doc ]) t
                                , [ G.fromString "]" ]
                                ]
                            )

        TupleValue a b ->
            G.group <|
                breakable
                    [ unbreakable [ G.fromString "(", toDocumentHelp False a ]
                    , unbreakable [ G.fromString ",", G.nest 2 <| toDocumentHelp False b ]
                    , G.fromString ")"
                    ]

        TripleValue a b c ->
            G.group <|
                breakable
                    [ unbreakable [ G.fromString "(", toDocumentHelp False a ]
                    , unbreakable [ G.fromString ",", G.nest 2 <| toDocumentHelp False b ]
                    , unbreakable [ G.fromString ",", G.nest 2 <| toDocumentHelp False c ]
                    , G.fromString ")"
                    ]


breakable : List G.Document -> G.Document
breakable =
    G.join G.space


unbreakable : List G.Document -> G.Document
unbreakable =
    G.join (G.fromString " ")


parens : G.Document -> G.Document
parens inner =
    G.group <|
        breakable
            [ G.concat
                [ G.fromString "("
                , inner
                ]
            , G.fromString ")"
            ]


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
        |> Debug.log "TODO: learn how to escape strings properly"
