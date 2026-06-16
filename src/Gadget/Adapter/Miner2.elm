module Gadget.Adapter.Miner2 exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Gadget
import Gadget.IR as IR exposing (IR)
import List.Extra
import Set exposing (Set)


const_MAX_COMMON =
    10


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
                StringOb s ->
                    Just (f s)

                _ ->
                    Nothing
        )


string2 : String -> (String -> String -> Bool) -> Invariant
string2 name f =
    Binary name
        (\t1 t2 ->
            case ( t1, t2 ) of
                ( StringOb s1, StringOb s2 ) ->
                    Just (f s1 s2)

                _ ->
                    Nothing
        )


gatherStats : List Ob -> Stats
gatherStats obs =
    let
        help ob statsDict =
            Dict.merge
                (\path obType stats -> Dict.insert path (new obType) stats)
                (\path obType statsType stats -> Dict.insert path (add obType statsType) stats)
                (\path statsType stats -> stats)
                ob
                statsDict
                Dict.empty

        new obType =
            case obType of
                StringOb s ->
                    StringStats_
                        { minLength = String.length s
                        , maxLength = String.length s
                        , chars = Set.fromList (String.toList s)
                        , substrings = Set.singleton s
                        , common = Just (Dict.singleton s 1)
                        }

                IntOb i ->
                    IntStats_
                        { min = i
                        , max = i
                        , common = Just (Dict.singleton i 1)
                        }

        add obType statsType =
            case ( obType, statsType ) of
                ( StringOb str, StringStats_ stats ) ->
                    StringStats_
                        { stats
                            | minLength = min (String.length str) stats.minLength
                            , maxLength = max (String.length str) stats.maxLength
                            , chars = Set.intersect (Set.fromList (String.toList str)) stats.chars
                            , substrings = stats.substrings |> Debug.log "TODO"
                            , common =
                                Maybe.andThen
                                    (\common ->
                                        let
                                            newCommon =
                                                Dict.update str
                                                    (\maybe ->
                                                        case maybe of
                                                            Just total ->
                                                                Just (total + 1)

                                                            Nothing ->
                                                                Just 1
                                                    )
                                                    common
                                        in
                                        if Dict.size newCommon > const_MAX_COMMON then
                                            Nothing

                                        else
                                            Just newCommon
                                    )
                                    stats.common
                        }

                ( IntOb i, IntStats_ stats ) ->
                    IntStats_ stats

                _ ->
                    statsType
    in
    List.foldl help Dict.empty obs


type alias Stats =
    Dict Path StatsType


type StatsType
    = IntStats_ IntStats
    | StringStats_ StringStats



-- | SBool BoolStats
-- | SFloat FloatStats
-- | SChar CharStats


type alias BoolStats =
    { true : Int
    , false : Int
    }


type alias CharStats =
    { min : Int
    , max : Int
    , common : Maybe (Dict Char Int)
    }


type alias StringStats =
    { minLength : Int
    , maxLength : Int
    , chars : Set Char
    , substrings : Set String
    , common : Maybe (Dict String Int)
    }


type alias IntStats =
    { min : Int
    , max : Int
    , common : Maybe (Dict Int Int)
    }


type alias FloatStats =
    { min : Float
    , max : Float
    , common : Maybe (Dict Float Int)
    }


type alias Path =
    List Int


type ObType
    = StringOb String
    | IntOb Int


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

        stats =
            gatherStats obs

        test1sFromStats =
            makeTest1s stats

        test2sFromStats =
            makeTest2s obs stats

        testsFromInvariants =
            List.foldl (prune invariants) Dict.empty obs
                |> Dict.values
                |> List.concat
    in
    test1sFromStats ++ test2sFromStats ++ testsFromInvariants


makeTest2s : List Ob -> Stats -> List Test
makeTest2s obs stats =
    stats
        |> Dict.toList
        |> List.concatMap
            (\( path, stat ) ->
                case stat of
                    StringStats_ stringStats ->
                        case stringStats.common of
                            Just common ->
                                common
                                    |> Dict.filter (\_ v -> v > 1)
                                    |> Dict.keys
                                    |> List.concatMap
                                        (\commonString ->
                                            -- now filter obs and check other fields when this field has a fixed value
                                            List.filter (\ob -> Dict.get path ob == Just (StringOb commonString)) obs
                                                |> gatherStats
                                                |> makeTest1s
                                                |> List.map
                                                    (\test_ ->
                                                        let
                                                            newLabel s =
                                                                "When " ++ pathToString path ++ " is \"" ++ commonString ++ "\", " ++ s
                                                        in
                                                        case test_ of
                                                            Test1 s p f ->
                                                                Test1 (newLabel s) p f

                                                            Test2 s p1 p2 f ->
                                                                Test2 (newLabel s) p1 p2 f
                                                    )
                                        )

                            Nothing ->
                                []

                    _ ->
                        []
            )


makeTest1s : Stats -> List Test
makeTest1s stats =
    stats
        |> Dict.toList
        |> List.concatMap
            (\( path, stat ) ->
                case stat of
                    StringStats_ stringStats ->
                        stringLengthTests path stringStats
                            ++ (case stringStats.common of
                                    Nothing ->
                                        []

                                    Just common ->
                                        [ stringTest1 path
                                            ("Is one of the " ++ String.fromInt (Dict.size common) ++ " Strings observed at this value")
                                            (\str -> Dict.member str common)
                                        ]
                               )

                    IntStats_ s ->
                        []
            )


stringTest1 : Path -> String -> (String -> Bool) -> Test
stringTest1 path text predicate =
    Test1 text
        path
        (\ob ->
            case Dict.get path ob of
                Just (StringOb str) ->
                    Just (predicate str)

                _ ->
                    Nothing
        )


stringLengthTests : Path -> StringStats -> List Test
stringLengthTests path { maxLength, minLength } =
    if maxLength == minLength then
        [ stringTest1 path
            ("Length is exactly " ++ String.fromInt maxLength)
            (\str -> String.length str == maxLength)
        ]

    else
        [ stringTest1 path
            ("Length is at most " ++ String.fromInt maxLength)
            (\str -> String.length str <= maxLength)
        , stringTest1 path
            ("Length is at least " ++ String.fromInt minLength)
            (\str -> String.length str >= minLength)
        ]


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
        StringOb s ->
            "\"" ++ s ++ "\""

        IntOb i ->
            String.fromInt i


pathToString : Path -> String
pathToString path =
    path
        |> List.reverse
        |> List.map String.fromInt
        |> String.join "."


fromIR : IR.IR -> Ob
fromIR ir =
    fromIRHelp [ 0 ] ir Dict.empty


fromIRHelp : Path -> IR.IR -> Ob -> Ob
fromIRHelp path ir ob =
    case ir of
        IR.Bool b ->
            Debug.todo "case not implemented yet"

        IR.Int i ->
            Dict.insert path (IntOb i) ob

        IR.Float f ->
            Debug.todo "case not implemented yet"

        IR.Char c ->
            Debug.todo "case not implemented yet"

        IR.String s ->
            Dict.insert path (StringOb s) ob

        IR.Product fields ->
            List.Extra.indexedFoldl
                (\idx field out ->
                    fromIRHelp (idx :: path) field out
                )
                ob
                fields

        _ ->
            Debug.todo "case not implemented yet"
