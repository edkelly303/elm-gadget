module Gadget.Adapter.String exposing (print, parser)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

**Warning:** the functions in this module may not do what you expect!

This is not a full-blown parser and pretty-printer for Elm values. It just
converts between Elm values and a simple and fairly compact String
representation. I originally wrote this adapter because I wanted to be able to
turn Elm values into Strings so that I could hash them.

If you are looking for a pretty-printer, try the
[`Gadget.Adapter.Pretty`](Gadget-Adapter-Pretty) module.


## API

@docs print, parser

-}

import Gadget.IR as IR
import List.Extra
import Parser as P exposing ((|.), (|=), Parser)


{-| Convert an Elm value into a String.

    import Gadget
    import Gadget.Adapter.String

    printer =
        Gadget.Adapter.String.print (Gadget.list Gadget.int)

    printed =
        printer [ 1, 2, 3 ]

    printed --> "l[i(1),i(2),i(3)]"

-}
print : IR.Gadget a -> a -> String
print gadget value =
    IR.fromInput gadget value
        |> printAdapter


primitive : String -> String -> String
primitive typeTag typeInfo =
    typeTag ++ "(" ++ typeInfo ++ ")"


combinator : String -> String -> List IR.Value -> String
combinator typeTag typeInfo items =
    typeTag
        ++ typeInfo
        ++ "["
        ++ String.join "," (List.map printAdapter items)
        ++ "]"


printAdapter : IR.Value -> String
printAdapter irValue =
    case irValue of
        IR.UnitValue ->
            "0"

        IR.BoolValue b ->
            primitive "b"
                (if b then
                    "1"

                 else
                    "0"
                )

        IR.CharValue c ->
            "c" ++ quoteString (String.fromChar c)

        IR.StringValue s ->
            "s" ++ quoteString s

        IR.IntValue i ->
            primitive "i" (String.fromInt i)

        IR.FloatValue f ->
            primitive "f" (String.fromFloat f)

        IR.CustomValue selected ( _, variant ) ->
            let
                args =
                    case variant of
                        IR.Variant0Value ->
                            []

                        IR.Variant1Value arg ->
                            [ arg ]

                        IR.Variant2Value arg1 arg2 ->
                            [ arg1
                            , arg2
                            ]

                        IR.Variant3Value arg1 arg2 arg3 ->
                            [ arg1
                            , arg2
                            , arg3
                            ]

                        IR.Variant4Value arg1 arg2 arg3 arg4 ->
                            [ arg1
                            , arg2
                            , arg3
                            , arg4
                            ]

                        IR.Variant5Value arg1 arg2 arg3 arg4 arg5 ->
                            [ arg1
                            , arg2
                            , arg3
                            , arg4
                            , arg5
                            ]
            in
            combinator
                "v"
                (String.fromInt selected)
                args

        IR.RecordValue fields ->
            combinator
                "r"
                ""
                (List.map Tuple.second fields)

        IR.ListValue items ->
            combinator
                "l"
                ""
                items

        IR.PairValue a b ->
            combinator "2" "" [ a, b ]

        IR.TripleValue a b c ->
            combinator "3" "" [ a, b, c ]


{-| Create a Parser that will attempt to convert a String created by `print`
into an Elm value.

    import Gadget
    import Gadget.Adapter.String
    import Parser -- from `elm/parser`

    parser = Gadget.Adapter.String.parser (Gadget.list Gadget.int)

    parsed = Parser.run parser "l[i(1),i(2),i(3)]"

    parsed --> Ok [ 1, 2, 3 ]

-}
parser : IR.Gadget a -> Parser a
parser gadget =
    irParser (IR.irType gadget)
        |> P.andThen
            (\ir ->
                case IR.toOutput gadget ir of
                    Ok output ->
                        P.succeed output

                    Err e ->
                        P.problem (String.join "\n" (List.map .error e))
            )


irParser : IR.Type -> Parser IR.Value
irParser irType =
    case irType of
        IR.UnitType _ ->
            unitParser |> P.map (\() -> IR.UnitValue)

        IR.BoolType _ ->
            boolParser |> P.map IR.BoolValue

        IR.CharType _ ->
            charParser |> P.map IR.CharValue

        IR.StringType _ ->
            stringParser |> P.map IR.StringValue

        IR.IntType _ ->
            intParser |> P.map IR.IntValue

        IR.FloatType _ ->
            floatParser |> P.map IR.FloatValue

        IR.CustomType _ firstNamedVariantType restNamedVariantTypes ->
            customParser firstNamedVariantType restNamedVariantTypes

        IR.RecordType _ namedFieldTypes ->
            recordParser namedFieldTypes

        IR.ListType _ _ ->
            Debug.todo "branch 'IR.ListType _ _' not implemented"

        IR.LazyType _ _ ->
            Debug.todo "branch 'IR.LazyType _ _' not implemented"

        IR.PairType _ _ _ ->
            Debug.todo "branch 'IR.PairType _ _ _' not implemented"

        IR.TripleType _ _ _ _ ->
            Debug.todo "branch 'IR.TripleType _ _ _ _' not implemented"


unitParser : Parser ()
unitParser =
    P.chompIf (\c -> c == '0')


floatParser : Parser Float
floatParser =
    primitiveParser "f" rawFloatParser


boolParser : Parser Bool
boolParser =
    primitiveParser "b" rawBoolParser


intParser : Parser Int
intParser =
    primitiveParser "i" rawIntParser


listParser : IR.Type -> Parser IR.Value
listParser irType =
    P.sequence
        { start = "l["
        , item = P.lazy (\() -> irParser irType)
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , trailing = P.Forbidden
        }
        |> P.map IR.ListValue


tupleParser : IR.Type -> Parser IR.Value
tupleParser irType =
    P.sequence
        { start = "2["
        , item = P.lazy (\() -> irParser irType)
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\values ->
                case values of
                    [ a, b ] ->
                        P.succeed (IR.PairValue a b)

                    _ ->
                        P.problem "wrong number of members for pair"
            )


tripleParser : IR.Type -> Parser IR.Value
tripleParser irType =
    P.sequence
        { start = "3["
        , item = P.lazy (\() -> irParser irType)
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\values ->
                case values of
                    [ a, b, c ] ->
                        P.succeed (IR.TripleValue a b c)

                    _ ->
                        P.problem "wrong number of members for triple"
            )


recordParser : List ( String, IR.Type ) -> Parser IR.Value
recordParser namedFieldTypes =
    P.succeed IR.RecordValue
        |. P.token "r["
        |= P.loop ( namedFieldTypes, [] ) fieldParser
        |. P.token "]"


fieldParser :
    ( List ( a, IR.Type ), List ( a, IR.Value ) )
    -> Parser (P.Step ( List ( a, IR.Type ), List ( a, IR.Value ) ) (List ( a, IR.Value )))
fieldParser ( namedFieldTypes, out ) =
    case namedFieldTypes of
        ( fieldName, fieldType ) :: rest ->
            P.succeed (\new -> P.Loop ( rest, ( fieldName, new ) :: out ))
                |= P.lazy (\() -> irParser fieldType)

        [] ->
            P.succeed () |> P.map (\_ -> P.Done out)


customParser : ( String, IR.VariantType ) -> List ( String, IR.VariantType ) -> Parser IR.Value
customParser firstNamedVariantType restNamedVariantTypes =
    P.succeed identity
        |. P.token "v"
        |= P.int
        |> P.andThen
            (\selected ->
                case List.Extra.getAt selected (firstNamedVariantType :: restNamedVariantTypes) of
                    Just ( variantName, variantType ) ->
                        let
                            variantValue =
                                case variantType of
                                    IR.Variant0Type ->
                                        P.succeed IR.Variant0Value
                                            |. P.symbol "]"

                                    IR.Variant1Type arg1Type ->
                                        P.succeed IR.Variant1Value
                                            |. P.symbol "["
                                            |= P.lazy (\() -> irParser arg1Type)
                                            |. P.symbol "]"

                                    IR.Variant2Type arg1Type arg2Type ->
                                        P.succeed IR.Variant2Value
                                            |. P.symbol "["
                                            |= P.lazy (\() -> irParser arg1Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg2Type)
                                            |. P.symbol "]"

                                    IR.Variant3Type arg1Type arg2Type arg3Type ->
                                        P.succeed IR.Variant3Value
                                            |. P.symbol "["
                                            |= P.lazy (\() -> irParser arg1Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg2Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg3Type)
                                            |. P.symbol "]"

                                    IR.Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                                        P.succeed IR.Variant4Value
                                            |. P.symbol "["
                                            |= P.lazy (\() -> irParser arg1Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg2Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg3Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg4Type)
                                            |. P.symbol "]"

                                    IR.Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                                        P.succeed IR.Variant5Value
                                            |. P.symbol "["
                                            |= P.lazy (\() -> irParser arg1Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg2Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg3Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg4Type)
                                            |. P.symbol ","
                                            |= P.lazy (\() -> irParser arg5Type)
                                            |. P.symbol "]"
                        in
                        P.map (\v -> IR.CustomValue selected ( variantName, v )) variantValue

                    Nothing ->
                        P.problem ("Selected variant #" ++ String.fromInt selected ++ " does not exist")
            )


primitiveParser : String -> Parser keep -> Parser keep
primitiveParser marker innerParser =
    P.succeed identity
        |. P.token (marker ++ "(")
        |= innerParser
        |. P.token ")"


rawBoolParser : Parser Bool
rawBoolParser =
    P.int
        |> P.andThen
            (\int ->
                case int of
                    0 ->
                        P.succeed False

                    1 ->
                        P.succeed True

                    _ ->
                        P.problem "Not a bool"
            )


charParser : Parser Char
charParser =
    (P.succeed identity
        |. P.token "c"
        |= rawStringParser
    )
        |> P.andThen
            (\str ->
                case String.uncons str of
                    Nothing ->
                        P.problem "Not a char"

                    Just ( c, _ ) ->
                        P.succeed c
            )


rawIntParser : Parser Int
rawIntParser =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.int
        , P.int
        ]


{-| In our case, we can't use `P.float` from elm/parser because it has a
bug with very large numbers - see <https://github.com/elm/parser/issues/58>.

This implementation is probably slower and won't handle things like

    > 1.79*10.0^308
    1.79e+308 : Float
    > String.fromFloat(1.79*10.0^308)
    "1.79e+308" : String

But that's ok in our case, because we don't need to handle Float literals, only
values produced by `String.fromFloat`.

-}
rawFloatParser : Parser Float
rawFloatParser =
    P.oneOf
        [ P.token "Infinity" |> P.map (\_ -> 1 / 0)
        , P.token "-Infinity" |> P.map (\_ -> -1 / 0)
        , P.token "NaN" |> P.map (\_ -> 0 / 0)
        , P.succeed negate
            |. P.symbol "-"
            |= floatParserHelp
        , floatParserHelp
        ]


floatParserHelp : Parser Float
floatParserHelp =
    let
        oneOrMoreDigits =
            P.succeed ()
                |. P.chompIf Char.isDigit
                |. P.chompWhile Char.isDigit
    in
    P.succeed
        (\start { usesENotation } end source ->
            { chompedString = String.slice start end source
            , usesENotation = usesENotation
            }
        )
        |= P.getOffset
        |. oneOrMoreDigits
        |= P.oneOf
            [ P.succeed identity
                |. P.chompIf (\c -> c == '.')
                |. oneOrMoreDigits
                |= P.oneOf
                    [ -- it's a number like `1.01e+21`
                      P.succeed { usesENotation = True }
                        |. P.chompIf (\c -> c == 'e')
                        |. P.oneOf
                            [ P.chompIf (\c -> c == '+')
                            , P.chompIf (\c -> c == '-')
                            ]
                        |. oneOrMoreDigits

                    -- it's a number like `1.1`
                    , P.succeed { usesENotation = False }
                    ]

            -- it's a number like `1`
            , P.succeed { usesENotation = False }
            ]
        |= P.getOffset
        |= P.getSource
        |> P.andThen
            (\{ chompedString, usesENotation } ->
                if usesENotation then
                    -- bail out and use `P.float` instead
                    case P.run P.float chompedString of
                        Ok f ->
                            P.succeed f

                        Err _ ->
                            P.problem "Not a float"

                else
                    -- just use `String.toFloat`
                    case String.toFloat chompedString of
                        Just f ->
                            P.succeed f

                        Nothing ->
                            P.problem "Not a Float"
            )


{-| The original version of this function comes from
<https://github.com/myrho/elm-parser-extras/tree/1.0.1> but the implementation
there seems to have a bug with an unguarded `chompWhile` leading to infinite
looping. This is a fixed version with all the generic stuff taken out so it just
works for our use case.
-}
stringParser : Parser String
stringParser =
    P.succeed identity
        |. P.token "s"
        |= rawStringParser


rawStringParser : Parser String
rawStringParser =
    P.succeed identity
        |. P.chompIf (\c -> c == '(')
        |= P.loop "" rawStringParserHelp


rawStringParserHelp : String -> Parser (P.Step String String)
rawStringParserHelp string =
    P.oneOf
        [ P.token "/)"
            |> P.map (\_ -> string ++ ")" |> P.Loop)
        , P.token "//"
            |> P.map (\_ -> string ++ "/" |> P.Loop)
        , P.chompIf ((==) ')')
            |> P.map (\_ -> P.Done string)
        , P.succeed ()
            |. P.chompIf (\c -> c /= ')' && c /= '/')
            |. P.chompWhile (\c -> c /= ')' && c /= '/')
            |> P.getChompedString
            |> P.map (\s -> string ++ s |> P.Loop)
        ]


quoteString : String -> String
quoteString str =
    let
        quoted =
            str
                |> String.replace "/" "//"
                |> String.replace ")" "/)"
    in
    "(" ++ quoted ++ ")"
