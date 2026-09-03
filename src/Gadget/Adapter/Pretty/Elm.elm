module Gadget.Adapter.Pretty.Elm exposing (toDocument)

import Gadget.IR as IR exposing (Value(..), VariantValue(..))
import Lib.Glam as G


toDocument : Value -> G.Document
toDocument =
    toDocumentHelp False False


toDocumentHelp : Bool -> Bool -> IR.Value -> G.Document
toDocumentHelp isChildOfCustomType isNested value =
    let
        nestIfNeeded =
            if isNested then
                G.nest 2

            else
                identity
    in
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
                                    toDocumentHelp True False arg1

                                Variant2Value arg1 arg2 ->
                                    breakable
                                        [ toDocumentHelp True False arg1
                                        , toDocumentHelp True False arg2
                                        ]

                                Variant3Value arg1 arg2 arg3 ->
                                    breakable
                                        [ toDocumentHelp True False arg1
                                        , toDocumentHelp True False arg2
                                        , toDocumentHelp True False arg3
                                        ]

                                Variant4Value arg1 arg2 arg3 arg4 ->
                                    breakable
                                        [ toDocumentHelp True False arg1
                                        , toDocumentHelp True False arg2
                                        , toDocumentHelp True False arg3
                                        , toDocumentHelp True False arg4
                                        ]

                                Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                                    breakable
                                        [ toDocumentHelp True False arg1
                                        , toDocumentHelp True False arg2
                                        , toDocumentHelp True False arg3
                                        , toDocumentHelp True False arg4
                                        , toDocumentHelp True False arg5
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
                                        , toDocumentHelp False False fld
                                        ]
                    in
                    G.group <|
                        nestIfNeeded <|
                            G.concat
                                (List.concat
                                    [ [ unbreakable [ G.fromString "{", printField h ] ]
                                    , List.map (\doc -> G.concat [ G.softBreak, G.fromString ", ", printField doc ]) t
                                    , [ G.space
                                      , G.fromString "}"
                                      ]
                                    ]
                                )

        ListValue items ->
            case items of
                [] ->
                    G.fromString "[]"

                h :: t ->
                    G.group <|
                        nestIfNeeded <|
                            G.concat
                                (List.concat
                                    [ [ unbreakable [ G.fromString "[", toDocumentHelp False True h ]
                                      ]
                                    , List.map
                                        (\doc ->
                                            G.concat
                                                [ G.softBreak
                                                , G.fromString ", "
                                                , toDocumentHelp False True doc
                                                ]
                                        )
                                        t
                                    , [ G.space
                                      , G.fromString "]"
                                      ]
                                    ]
                                )

        TupleValue a b ->
            G.group <|
                nestIfNeeded <|
                    G.concat
                        [ unbreakable [ G.fromString "(", toDocumentHelp False True a ]
                        , G.softBreak
                        , unbreakable [ G.fromString ",", toDocumentHelp False True b ]
                        , G.space
                        , G.fromString ")"
                        ]

        TripleValue a b c ->
            G.group <|
                nestIfNeeded <|
                    G.concat
                        [ unbreakable [ G.fromString "(", toDocumentHelp False True a ]
                        , G.softBreak
                        , unbreakable [ G.fromString ",", toDocumentHelp False True b ]
                        , G.softBreak
                        , unbreakable [ G.fromString ",", toDocumentHelp False True c ]
                        , G.space
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
        G.concat
            [ G.fromString "("
            , inner
            , G.softBreak
            , G.fromString ")"
            ]


escape : String -> String
escape s =
    s
        |> String.replace "\"" "\\\""
        |> String.replace "\t" "\\t"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\n" "\\n"
        --|> Debug.log "TODO: learn how to escape strings properly"
