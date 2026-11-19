module Gadget.Adapter.Default exposing
    ( Config
    , config
    , default
    , defaultWithConfig
    )

import Gadget.IR as IR exposing (Gadget, Type(..), Value(..), VariantType(..), VariantValue(..))


type alias Config =
    { int : Int
    , float : Float
    , string : String
    , char : Char
    , bool : Bool
    }


config : Config
config =
    { int = 0
    , float = 0
    , string = ""
    , char = ' '
    , bool = False
    }


default : IR.Gadget a -> IR.Value
default =
    defaultWithConfig config


defaultWithConfig : Config -> Gadget a -> Value
defaultWithConfig config_ gadget =
    defaultWithConfigHelp config_ (IR.irType gadget)


defaultWithConfigHelp : Config -> Type -> Value
defaultWithConfigHelp config_ type_ =
    case type_ of
        UnitType _ ->
            UnitValue

        IntType _ ->
            IntValue config_.int

        BoolType _ ->
            BoolValue config_.bool

        CharType _ ->
            CharValue config_.char

        StringType _ ->
            StringValue config_.string

        FloatType _ ->
            FloatValue config_.float

        CustomType _ ( firstVariantName, firstVariantType ) _ ->
            CustomValue 0
                ( firstVariantName
                , case firstVariantType of
                    Variant0Type ->
                        Variant0Value

                    Variant1Type arg1Type ->
                        Variant1Value
                            (defaultWithConfigHelp config_ arg1Type)

                    Variant2Type arg1Type arg2Type ->
                        Variant2Value
                            (defaultWithConfigHelp config_ arg1Type)
                            (defaultWithConfigHelp config_ arg2Type)

                    Variant3Type arg1Type arg2Type arg3Type ->
                        Variant3Value
                            (defaultWithConfigHelp config_ arg1Type)
                            (defaultWithConfigHelp config_ arg2Type)
                            (defaultWithConfigHelp config_ arg3Type)

                    Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                        Variant4Value
                            (defaultWithConfigHelp config_ arg1Type)
                            (defaultWithConfigHelp config_ arg2Type)
                            (defaultWithConfigHelp config_ arg3Type)
                            (defaultWithConfigHelp config_ arg4Type)

                    Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                        Variant5Value
                            (defaultWithConfigHelp config_ arg1Type)
                            (defaultWithConfigHelp config_ arg2Type)
                            (defaultWithConfigHelp config_ arg3Type)
                            (defaultWithConfigHelp config_ arg4Type)
                            (defaultWithConfigHelp config_ arg5Type)
                )

        RecordType _ namedFieldTypes ->
            RecordValue
                (List.map
                    (\( fieldName, fieldType ) ->
                        ( fieldName, defaultWithConfigHelp config_ fieldType )
                    )
                    namedFieldTypes
                )

        ListType _ _ ->
            ListValue []

        PairType _ a b ->
            PairValue
                (defaultWithConfigHelp config_ a)
                (defaultWithConfigHelp config_ b)

        TripleType _ a b c ->
            TripleValue
                (defaultWithConfigHelp config_ a)
                (defaultWithConfigHelp config_ b)
                (defaultWithConfigHelp config_ c)

        LazyType _ thunk ->
            defaultWithConfigHelp config_ (thunk ())
