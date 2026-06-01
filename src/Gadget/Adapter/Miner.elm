module Gadget.Adapter.Miner exposing (..)

import Dict exposing (Dict)
import Gadget
import Gadget.Adapter.Random
import Gadget.IR as IR exposing (Gadget, IR, IRType)
import Html as H
import List.Extra
import Random
import Set exposing (Set)


type Ob
    = Bool BoolOb
    | Char ()
    | String StringOb
    | Int IntOb
    | Float FloatOb
    | Custom Int Variant
    | Product (List Ob)
    | List { minLength : Int, maxLength : Int } Ob
    | Labelled (Set String) Ob


obGadget : Gadget.Gadget Ob
obGadget =
    Gadget.custom
        (\bool string int float product variant ->
            case variant of
                Bool b ->
                    bool b

                String s ->
                    string s

                Int i ->
                    int i

                Float f ->
                    float f

                Product f ->
                    product f

                _ ->
                    Debug.todo "oops"
        )
        |> Gadget.variant1 Bool boolObGadget
        |> Gadget.variant1 String stringObGadget
        |> Gadget.variant1 Int intObGadget
        |> Gadget.variant1 Float floatObGadget
        |> Gadget.variant1 Product (Gadget.list (Gadget.lazy (\() -> obGadget)))
        |> Gadget.endCustom


type alias BoolOb =
    { true : Int, false : Int }


boolObGadget : Gadget.Gadget { false : Int, true : Int }
boolObGadget =
    Gadget.record BoolOb
        |> Gadget.field .true Gadget.int
        |> Gadget.field .false Gadget.int
        |> Gadget.endRecord


type alias StringOb =
    { minLength : Int
    , maxLength : Int
    , chars : Maybe (Set Char)
    , substrings : Maybe (Set String)
    , common : Maybe (Dict String Int)
    }


stringObGadget : Gadget.Gadget { common : Maybe (Dict String Int), substrings : Maybe (Set String), chars : Maybe (Set Char), maxLength : Int, minLength : Int }
stringObGadget =
    Gadget.record StringOb
        |> Gadget.field .minLength Gadget.int
        |> Gadget.field .maxLength Gadget.int
        |> Gadget.field .chars (Gadget.maybe (Gadget.set Gadget.char))
        |> Gadget.field .substrings (Gadget.maybe (Gadget.set Gadget.string))
        |> Gadget.field .common (Gadget.maybe (Gadget.dict Gadget.string Gadget.int))
        |> Gadget.endRecord


type alias IntOb =
    { min : Int, max : Int }


intObGadget : Gadget.Gadget { max : Int, min : Int }
intObGadget =
    Gadget.record IntOb
        |> Gadget.field .min Gadget.int
        |> Gadget.field .max Gadget.int
        |> Gadget.endRecord


type alias FloatOb =
    { min : Float, max : Float }


floatObGadget : Gadget.Gadget { max : Float, min : Float }
floatObGadget =
    Gadget.record FloatOb
        |> Gadget.field .min Gadget.float
        |> Gadget.field .max Gadget.float
        |> Gadget.endRecord


zero : Gadget a -> Ob
zero gadget =
    let
        help irType =
            case irType of
                IR.BoolType ->
                    Bool { true = 0, false = 0 }

                IR.StringType ->
                    String
                        { minLength = Random.maxInt
                        , maxLength = 0
                        , chars = Nothing
                        , substrings = Nothing
                        , common = Just Dict.empty
                        }

                IR.CharType ->
                    Char ()

                IR.IntType ->
                    Int
                        { min = Random.maxInt
                        , max = Random.minInt
                        }

                IR.FloatType ->
                    Float
                        { min = 1 / 0
                        , max = -1 / 0
                        }

                IR.CustomType _ _ ->
                    Debug.todo "branch 'CustomType _ _' not implemented"

                IR.ProductType fieldTypes ->
                    Product (List.map help fieldTypes)

                IR.ListType _ ->
                    Debug.todo "branch 'ListType _' not implemented"

                IR.LabelledType _ _ ->
                    Debug.todo "branch 'LabelledType _ _' not implemented"

                IR.LazyType _ ->
                    Debug.todo "branch LazyType not implemented"
    in
    help (IR.irType gadget)


{-| A type used by the `Custom` constructor of the `Ob` type.
-}
type Variant
    = Variant0
    | Variant1 Ob
    | Variant2 Ob Ob
    | Variant3 Ob Ob Ob
    | Variant4 Ob Ob Ob Ob
    | Variant5 Ob Ob Ob Ob Ob


observe : IR.Gadget a -> List a -> Ob
observe gadget values =
    values
        |> List.map (IR.fromInput gadget)
        |> List.map fromValue
        |> List.foldl append (zero gadget)


fromValue : IR -> Ob
fromValue ir =
    case ir of
        IR.Bool i ->
            Bool <|
                if i then
                    { true = 1, false = 0 }

                else
                    { true = 0, false = 1 }

        IR.Char _ ->
            Char ()

        IR.String s ->
            let
                len =
                    String.length s
            in
            String
                { minLength = len
                , maxLength = len
                , chars =
                    String.toList s
                        |> Set.fromList
                        |> Just
                , substrings =
                    Just (Set.singleton s)
                , common =
                    Just (Dict.singleton s 1)
                }

        IR.Int i ->
            Int
                { min = i
                , max = i
                }

        IR.Float f ->
            Float
                { min = f
                , max = f
                }

        IR.Custom _ _ ->
            Debug.todo "branch '( Custom _ _, _ )' not implemented"

        IR.Product fields ->
            List.map fromValue fields
                |> Product

        IR.List _ ->
            Debug.todo "branch '( List _, _ )' not implemented"

        IR.Labelled _ _ ->
            Debug.todo "branch '( Labelled _ _, _ )' not implemented"


append : Ob -> Ob -> Ob
append ob1 ob2 =
    case ( ob1, ob2 ) of
        ( Bool b1, Bool b2 ) ->
            Bool
                { true = b1.true + b2.true
                , false = b1.false + b2.false
                }

        ( Char c1, Char c2 ) ->
            Char ()

        ( String s1, String s2 ) ->
            String
                { minLength = min s1.minLength s2.minLength
                , maxLength = max s1.maxLength s2.maxLength
                , chars =
                    case ( s1.chars, s2.chars ) of
                        ( Nothing, Nothing ) ->
                            Nothing

                        ( Nothing, Just chars ) ->
                            Just chars

                        ( Just chars, Nothing ) ->
                            Just chars

                        ( Just chars1, Just chars2 ) ->
                            Just (Set.intersect chars1 chars2)
                , substrings =
                    case ( s1.substrings, s2.substrings ) of
                        ( Nothing, Nothing ) ->
                            Nothing

                        ( Nothing, Just substrings ) ->
                            Just substrings

                        ( Just substrings, Nothing ) ->
                            Just substrings

                        ( Just substrings1, Just substrings2 ) ->
                            substrings1
                                |> Set.toList
                                |> List.concatMap (\sub -> Set.toList substrings2 |> List.map (\sub2 -> longestCommonSubstring sub sub2))
                                |> Set.fromList
                                |> Just
                , common =
                    case ( s1.common, s2.common ) of
                        ( Nothing, Nothing ) ->
                            Nothing

                        ( Nothing, Just common ) ->
                            Nothing

                        ( Just common, Nothing ) ->
                            Nothing

                        ( Just common1, Just common2 ) ->
                            let
                                new =
                                    Dict.merge
                                        (\k v1 out -> Dict.insert k v1 out)
                                        (\k v1 v2 out -> Dict.insert k (v1 + v2) out)
                                        (\k v2 out -> Dict.insert k v2 out)
                                        common1
                                        common2
                                        Dict.empty
                            in
                            if Dict.size new >= 100 then
                                Nothing

                            else
                                Just new
                }

        ( Int i1, Int i2 ) ->
            Int { max = max i1.max i2.max, min = min i1.min i2.min }

        ( Float f1, Float f2 ) ->
            Float { max = max f1.max f2.max, min = min f1.min f2.min }

        ( Product fields1, Product fields2 ) ->
            Product <| List.map2 append fields1 fields2

        _ ->
            ob1


longestCommonSubstring : String -> String -> String
longestCommonSubstring s1 s2 =
    lcs (String.toList s1) (String.toList s2)
        |> String.fromList


{-|

    Longest Common Subsequence implementation found on StackOverflow, credited to Gilbert Kennen
    https://stackoverflow.com/questions/46183247/longest-common-subsequence-in-elm-with-memoization

-}
lcs : List a -> List a -> List a
lcs xs ys =
    lcsHelper xs ys ( 0, 0 ) Dict.empty
        |> Dict.get ( 0, 0 )
        |> Maybe.map Tuple.second
        |> Maybe.withDefault []


lcsHelper : List a -> List a -> ( Int, Int ) -> Dict ( Int, Int ) ( Int, List a ) -> Dict ( Int, Int ) ( Int, List a )
lcsHelper list1 list2 position memo =
    case ( Dict.get position memo, list1, list2 ) of
        ( Nothing, item1 :: rest1, item2 :: rest2 ) ->
            let
                nextList2Pos =
                    Tuple.mapSecond ((+) 1) position

                nextList1Pos =
                    Tuple.mapFirst ((+) 1) position

                newMemo =
                    memo
                        |> lcsHelper list1 rest2 nextList2Pos
                        |> lcsHelper rest1 list2 nextList1Pos

                best =
                    maxListTuple
                        (get nextList1Pos newMemo)
                        (get nextList2Pos newMemo)
                        |> consIfEqual item1 item2
            in
            Dict.insert position best newMemo

        _ ->
            memo


get : ( Int, Int ) -> Dict ( Int, Int ) ( Int, List a ) -> ( Int, List a )
get position memo =
    Dict.get position memo |> Maybe.withDefault ( 0, [] )


maxListTuple : ( Int, List a ) -> ( Int, List a ) -> ( Int, List a )
maxListTuple ( list1Len, list1 ) ( list2Len, list2 ) =
    if list2Len > list1Len then
        ( list2Len, list2 )

    else
        ( list1Len, list1 )


consIfEqual : a -> a -> ( Int, List a ) -> ( Int, List a )
consIfEqual item1 item2 ( listLen, list ) =
    if item1 == item2 then
        ( listLen + 1, item1 :: list )

    else
        ( listLen, list )


view : Ob -> H.Html msg
view ob =
    case ob of
        Labelled labels inner ->
            H.div []
                [ H.strong [] [ H.text "Labelled" ]
                , H.text (Set.toList labels |> String.join ", ")
                , view inner
                ]

        Bool b ->
            H.div []
                [ H.strong [] [ H.text "Bool" ]
                , H.dl []
                    [ kv "True" (String.fromInt b.true)
                    , kv "False" (String.fromInt b.false)
                    ]
                ]

        Char c ->
            H.div [] [ H.strong [] [ H.text "Char" ] ]

        String s ->
            H.div []
                [ H.strong [] [ H.text "String" ]
                , H.dl []
                    [ kv "min length" (String.fromInt s.minLength)
                    , kv "max length" (String.fromInt s.maxLength)
                    , kv "values that appear frequently"
                        (case s.common of
                            Just common ->
                                if Dict.isEmpty common then
                                    "[none]"

                                else
                                    Dict.toList common
                                        |> List.filter (\( value, count ) -> count > 1)
                                        |> List.map (\( value, count ) -> "\"" ++ value ++ "\"" ++ ": " ++ String.fromInt count)
                                        |> String.join ", "

                            Nothing ->
                                "[more than 100 different values]"
                        )
                    , kv "characters present in all values"
                        (case s.chars of
                            Just chars ->
                                if Set.isEmpty chars then
                                    "[none]"

                                else
                                    Set.toList chars
                                        |> List.map String.fromChar
                                        |> List.map (\str -> "'" ++ str ++ "'")
                                        |> String.join ", "

                            Nothing ->
                                "[none]"
                        )
                    , kv "substrings present in all values"
                        (case s.substrings of
                            Just substrings ->
                                if Set.isEmpty substrings then
                                    "[none]"

                                else
                                    Set.toList substrings
                                        |> List.map (\str -> "\"" ++ str ++ "\"")
                                        |> String.join ", "

                            Nothing ->
                                "[none]"
                        )
                    ]
                ]

        Int i ->
            H.div []
                [ H.strong [] [ H.text "Int" ]
                , H.dl []
                    [ kv "min" (String.fromInt i.min)
                    , kv "max" (String.fromInt i.max)
                    ]
                ]

        Float f ->
            H.div []
                [ H.strong [] [ H.text "Float" ]
                , H.dl []
                    [ kv "min" (String.fromFloat f.min)
                    , kv "max" (String.fromFloat f.max)
                    ]
                ]

        Custom _ _ ->
            H.div [] [ H.strong [] [ H.text "Custom" ] ]

        Product fields ->
            H.div []
                [ H.strong [] [ H.text "Product" ]
                , H.dl []
                    (List.indexedMap
                        (\idx field ->
                            H.div []
                                [ H.dt [] [ H.text (String.fromInt idx) ]
                                , H.dd [] [ view field ]
                                ]
                        )
                        fields
                    )
                ]

        List _ _ ->
            H.div [] [ H.strong [] [ H.text "List" ] ]


kv k v =
    H.div [] [ H.dt [] [ H.text k ], H.dd [] [ H.text v ] ]
