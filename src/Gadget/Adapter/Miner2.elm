module Gadget.Adapter.Miner2 exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Gadget
import Gadget.IR as IR exposing (IR)
import List.Extra


g =
    Gadget.tuple Gadget.string (Gadget.tuple Gadget.string Gadget.string)


exampleTests : List Test
exampleTests =
    observe g
        [ ( "", ( "1", "2" ) )
        , ( "", ( "hello", "2" ) )
        , ( "", ( "xxxxxxxxxxxxxx", "1" ) )
        ]


exampleResult : Result (List String) ( String, ( String, String ) )
exampleResult =
    test g exampleTests ( "x", ( "", "1234567890" ) )


invariants : List Invariant
invariants =
    [ string "String is always empty" String.isEmpty
    , string "String is always less than 10 characters" (\s -> String.length s < 10)
    , string2 "Arg 1 is always at least as long as arg2" (\s1 s2 -> String.length s1 >= String.length s2)
    ]


string : String -> (String -> Bool) -> Invariant
string name f =
    Unary name
        (\t ->
            case t of
                ObString s ->
                    Just (f s)

                _ ->
                    Nothing
        )


string2 : String -> (String -> String -> Bool) -> Invariant
string2 name f =
    Binary name
        (\t1 t2 ->
            case ( t1, t2 ) of
                ( ObString s1, ObString s2 ) ->
                    Just (f s1 s2)

                _ ->
                    Nothing
        )


type alias Path =
    List Int


type ObType
    = ObString String
    | ObInt Int


type alias Ob =
    Dict Path ObType


type Invariant
    = Unary String (ObType -> Maybe Bool)
    | Binary String (ObType -> ObType -> Maybe Bool)


type Test
    = Test1 String Path (Ob -> Maybe Bool)
    | Test2 String Path Path (Ob -> Maybe Bool)


observe : IR.Gadget a -> List a -> List Test
observe gadget list =
    let
        obs =
            List.map (IR.fromInput gadget >> fromIR) list
    in
    List.foldl (prune invariants) Dict.empty obs
        |> Dict.values
        |> List.concat


test : IR.Gadget a -> List Test -> a -> Result (List String) a
test gadget tests value =
    let
        ob =
            value
                |> IR.fromInput gadget
                |> fromIR

        errs =
            List.filterMap
                (\test_ ->
                    case test_ of
                        Test1 name path f ->
                            case f ob of
                                Just True ->
                                    Nothing

                                Just False ->
                                    Just
                                        ("The value at "
                                            ++ pathToString path
                                            ++ " is "
                                            ++ (Dict.get path ob |> Maybe.map obTypeToString |> Maybe.withDefault "")
                                            ++ ", which breaks the invariant: "
                                            ++ name
                                        )

                                Nothing ->
                                    Nothing

                        Test2 name path1 path2 f ->
                            case f ob of
                                Just True ->
                                    Nothing

                                Just False ->
                                    Just
                                        ("The values at "
                                            ++ pathToString path1
                                            ++ " and "
                                            ++ pathToString path2
                                            ++ " are "
                                            ++ (Dict.get path1 ob |> Maybe.map obTypeToString |> Maybe.withDefault "")
                                            ++ " and "
                                            ++ (Dict.get path2 ob |> Maybe.map obTypeToString |> Maybe.withDefault "")
                                            ++ ", which breaks the invariant: "
                                            ++ name
                                        )

                                Nothing ->
                                    Nothing
                )
                tests
    in
    if List.isEmpty errs then
        Ok value

    else
        Err errs


obTypeToString : ObType -> String
obTypeToString obType =
    case obType of
        ObString s ->
            "\"" ++ s ++ "\""

        ObInt i ->
            String.fromInt i


pathToString : Path -> String
pathToString path =
    path
        |> List.reverse
        |> List.map String.fromInt
        |> String.join "."


prune : List Invariant -> Ob -> Dict Path (List Test) -> Dict Path (List Test)
prune invariants_ ob out =
    let
        validUnaries =
            Dict.map
                (\path obType ->
                    List.filterMap
                        (\inv ->
                            case inv of
                                Unary name f ->
                                    case f obType of
                                        Just False ->
                                            Nothing

                                        _ ->
                                            Just (Test1 name path (\ob_ -> Maybe.andThen f (Dict.get path ob_)))

                                _ ->
                                    Nothing
                        )
                        invariants
                )
                ob

        pathPairs =
            List.Extra.uniquePairs (Dict.keys ob)

        validBinaries =
            List.foldl
                (\( path1, path2 ) out_ ->
                    let
                        tests =
                            List.filterMap
                                (\inv ->
                                    case inv of
                                        Binary name f ->
                                            Maybe.map2
                                                (\obType1 obType2 ->
                                                    case f obType1 obType2 of
                                                        Just False ->
                                                            Nothing

                                                        _ ->
                                                            Just
                                                                (Test2 name
                                                                    path1
                                                                    path2
                                                                    (\ob_ ->
                                                                        Maybe.map2 f (Dict.get path1 ob_) (Dict.get path2 ob_)
                                                                            |> Maybe.andThen identity
                                                                    )
                                                                )
                                                )
                                                (Dict.get path1 ob)
                                                (Dict.get path2 ob)
                                                |> Maybe.andThen identity

                                        _ ->
                                            Nothing
                                )
                                invariants
                    in
                    Dict.insert path1 tests out_
                )
                Dict.empty
                pathPairs
    in
    Dict.merge (\p u d -> Dict.insert p u d)
        (\p u b d -> Dict.insert p (u ++ b) d)
        (\p b d -> Dict.insert p b d)
        validUnaries
        validBinaries
        Dict.empty


fromIR : IR.IR -> Ob
fromIR ir =
    fromIRHelp [ 0 ] ir Dict.empty


fromIRHelp : Path -> IR.IR -> Ob -> Ob
fromIRHelp path ir ob =
    case ir of
        IR.Bool b ->
            Debug.todo "case not implemented yet"

        IR.Int i ->
            Dict.insert path (ObInt i) ob

        IR.Float f ->
            Debug.todo "case not implemented yet"

        IR.Char c ->
            Debug.todo "case not implemented yet"

        IR.String s ->
            Dict.insert path (ObString s) ob

        IR.Product fields ->
            List.Extra.indexedFoldl
                (\idx field out ->
                    fromIRHelp (idx :: path) field out
                )
                ob
                fields

        _ ->
            Debug.todo "case not implemented yet"
