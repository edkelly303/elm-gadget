module Gadget.Adapter.Miner2 exposing (..)

import Dict exposing (Dict)
import Gadget
import Gadget.IR as IR exposing (IR)
import List.Extra


g =
    Gadget.tuple Gadget.string (Gadget.tuple Gadget.string Gadget.string)


example =
    observe g
        [ ( "", ( "", "" ) )
        , ( "", ( "hello", "" ) )
        , ( "", ( "xxxxxxxxxxxxxx", "1" ) )
        ]


stringInvs : List IString
stringInvs =
    [ String1 "String is empty" String.isEmpty
    , String1 "String is less than 10 characters" (\s -> String.length s < 10)
    ]


type alias Path =
    List Int


type alias Primitives s c i f b =
    { string : Dict Path s
    , char : Dict Path c
    , int : Dict Path i
    , float : Dict Path f
    , bool : Dict Path b
    }


type alias Ob =
    Primitives String Char Int Float Bool


type alias Invariants =
    Primitives (List IString) (List IChar) (List IInt) (List IFloat) (List IBool)


type IString
    = String1 String (String -> Bool)
    | String2 String (String -> String -> Bool)


type IInt
    = Int1 String (Int -> Bool)
    | Int2 String (Int -> Int -> Bool)


type alias IChar =
    ()


type alias IFloat =
    ()


type alias IBool =
    ()


observe : IR.Gadget a -> List a -> Invariants
observe gadget list =
    let
        obs =
            List.map (IR.fromInput gadget >> fromIR) list
    in
    List.foldl
        (\ob inv ->
            { inv
                | string =
                    Dict.merge
                        (\p obV out ->
                            Dict.insert p
                                (List.filter
                                    (\i ->
                                        case i of
                                            String1 _ f ->
                                                f obV

                                            String2 _ f ->
                                                True
                                    )
                                    stringInvs
                                )
                                out
                        )
                        (\p obV invV out ->
                            Dict.insert p
                                (List.filter
                                    (\i ->
                                        case i of
                                            String1 _ f ->
                                                f obV

                                            String2 _ _ ->
                                                True
                                    )
                                    invV
                                )
                                out
                        )
                        (\_ _ out -> out)
                        ob.string
                        inv.string
                        zero.string
            }
        )
        zero
        obs


zero : Primitives s c i f b
zero =
    { string = Dict.empty
    , char = Dict.empty
    , int = Dict.empty
    , float = Dict.empty
    , bool = Dict.empty
    }


fromIR : IR.IR -> Ob
fromIR ir =
    fromIRHelp [0] ir zero


fromIRHelp : Path -> IR.IR -> Ob -> Ob
fromIRHelp path ir ob =
    case ir of
        IR.Bool b ->
            { ob | bool = Dict.insert path b ob.bool }

        IR.Int i ->
            { ob | int = Dict.insert path i ob.int }

        IR.Float f ->
            { ob | float = Dict.insert path f ob.float }

        IR.Char c ->
            { ob | char = Dict.insert path c ob.char }

        IR.String s ->
            { ob | string = Dict.insert path s ob.string }

        IR.Product fields ->
            List.Extra.indexedFoldl
                (\idx field out ->
                    fromIRHelp (idx :: path) field out
                )
                ob
                fields

        _ ->
            Debug.todo "case not implemented yet"
