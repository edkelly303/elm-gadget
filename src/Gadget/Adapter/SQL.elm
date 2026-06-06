module Gadget.Adapter.SQL exposing (..)

import Gadget
import Gadget.IR as IR exposing (irType)
import List.Extra
import Set
import String.Extra


type alias Person =
    { name : String, age : Int, pets : List Pet }


personGadget =
    Gadget.record Person
        |> Gadget.field .name (Gadget.string |> Gadget.label "columnName:name")
        |> Gadget.field .age Gadget.int
        |> Gadget.field .pets (Gadget.list petGadget)
        |> Gadget.endRecord


type alias Pet =
    { name : String, isFriendly : Bool }


petGadget =
    Gadget.record Pet
        |> Gadget.field .name Gadget.string
        |> Gadget.field .isFriendly Gadget.bool
        |> Gadget.endRecord


example =
    createSchema personGadget


createSchema : IR.Gadget a -> String
createSchema gadget =
    let
        help path table out typ =
            case typ of
                IR.LazyType inner ->
                    help path table out (inner ())

                IR.BoolType ->
                    { column = columnToString path, table = table, type_ = "INTEGER" } :: out

                IR.CharType ->
                    { column = columnToString path, table = table, type_ = "TEXT" } :: out

                IR.StringType ->
                    { column = columnToString path, table = table, type_ = "TEXT" } :: out

                IR.IntType ->
                    { column = columnToString path, table = table, type_ = "INTEGER" } :: out

                IR.FloatType ->
                    { column = columnToString path, table = table, type_ = "REAL" } :: out

                IR.CustomType _ _ ->
                    Debug.todo "branch 'CustomType _ _' not implemented"

                IR.ProductType fields ->
                    List.foldl
                        (\field ( idx_, out_ ) ->
                            ( idx_ + 1, help (idx_ :: path) table out_ field )
                        )
                        ( 0, out )
                        fields
                        |> Tuple.second

                IR.ListType inner ->
                    help (0 :: path) (0 :: path) out inner ++ [ { column = "fkey", table = 0 :: path, type_ = "INTEGER" } ]

                IR.LabelledType labels inner ->
                    case help path table out inner of
                        [ one ] ->
                            [ { one
                                | column =
                                    labels
                                        |> Set.toList
                                        |> List.Extra.find (String.startsWith "columnName:")
                                        |> Maybe.map
                                            (String.Extra.rightOf "columnName:"
                                                >> String.trim
                                                >> String.replace "\"" ""
                                                >> String.Extra.surround "\""
                                            )
                                        |> Maybe.withDefault one.column
                              }
                            ]

                        many ->
                            many
    in
    help [] [ 0 ] [] (IR.irType gadget)
        |> List.Extra.gatherEqualsBy .table
        |> List.map
            (\( fst, rst ) ->
                "CREATE TABLE "
                    ++ tableToString fst.table
                    ++ " ("
                    ++ fst.column
                    ++ " "
                    ++ fst.type_
                    ++ " PRIMARY KEY,\n"
                    ++ String.join ",\n  " (List.Extra.reverseMap (\{ column, type_ } -> column ++ " " ++ type_) rst)
                    ++ "\n);"
            )
        |> String.join "\n\n"


type alias Table =
    { name : String
    , primaryKey : String
    , columns : List Column
    , relations : List Relation
    }


type alias Column =
    { name : String, type_ : String }


type alias Relation =
    { foreignKey : String
    , referencedTable : String
    , referencedColumn : String
    }


createSchema2 : IR.Gadget a -> List Table
createSchema2 gadget =
    createSchema2Help [] [] (IR.irType gadget)


createSchema2Help : List Int -> List Table -> IR.IRType -> List Table
createSchema2Help path tables irType =
    case irType of
        IR.BoolType ->
            handlePrimitive path "INTEGER" tables

        IR.CharType ->
            handlePrimitive path "TEXT" tables

        IR.StringType ->
            handlePrimitive path "TEXT" tables

        IR.IntType ->
            handlePrimitive path "INTEGER" tables

        IR.FloatType ->
            handlePrimitive path "REAL" tables

        IR.CustomType _ _ ->
            Debug.todo "branch 'CustomType _ _' not implemented"

        IR.ProductType fields ->
            handleProduct path fields tables

        IR.ListType inner ->
            handleList path inner tables

        IR.LabelledType _ _ ->
            Debug.todo "branch 'LabelledType _ _' not implemented"

        IR.LazyType _ ->
            Debug.todo "branch 'LazyType _' not implemented"


handlePrimitive : List Int -> String -> List Table -> List Table
handlePrimitive path type_ tables =
    case tables of
        [] ->
            [ { name = tableToString path
              , primaryKey = columnToString path
              , columns = [ { name = columnToString path, type_ = type_ } ]
              , relations = []
              }
            ]

        table :: restTables ->
            { table
                | columns = { name = columnToString path, type_ = type_ } :: table.columns
            }
                :: restTables


handleProduct : List Int -> List IR.IRType -> List Table -> List Table
handleProduct path fields tables =
    List.indexedMap (\idx field -> createSchema2Help (idx :: path) [] field) fields
        |> List.concat


handleList : List Int -> IR.IRType -> List Table -> List Table
handleList path inner tables =
    let
        ( referencedColumn, referencedTable ) =
            case tables of
                table :: restTables ->
                    ( table.primaryKey, table.name )

                _ ->
                    ( "ERROR!!", "ERROR!!" )
    in
    case createSchema2Help (0 :: path) [] inner of
        innerTable :: restInnerTables ->
            tables
                ++ ({ innerTable
                        | columns = { name = "fkey", type_ = "INTEGER" } :: innerTable.columns
                        , relations = { foreignKey = "fkey", referencedTable = referencedTable, referencedColumn = referencedColumn } :: innerTable.relations
                    }
                        :: restInnerTables
                   )

        [] ->
            tables


tableToString : List Int -> String
tableToString table =
    "\"table_"
        ++ (table
                |> List.map (\int -> String.fromInt int)
                |> List.reverse
                |> String.join "_"
           )
        ++ "\""


columnToString : List Int -> String
columnToString column =
    "\"column_"
        ++ (column
                |> List.map (\int -> String.fromInt int)
                |> List.reverse
                |> String.join "_"
           )
        ++ "\""
