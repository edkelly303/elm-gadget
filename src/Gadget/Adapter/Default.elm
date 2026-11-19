module Gadget.Adapter.Default exposing
    ( Config
    , config
    , forGadget
    , forGadgetWithConfig
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


forGadget : IR.Gadget a -> IR.Value
forGadget =
    forGadgetWithConfig config


forGadgetWithConfig : Config -> Gadget a -> Value
forGadgetWithConfig config_ gadget =
    forGadgetWithConfigHelp config_ (IR.irType gadget)


forGadgetWithConfigHelp : Config -> Type -> Value
forGadgetWithConfigHelp config_ type_ =
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
                            (forGadgetWithConfigHelp config_ arg1Type)

                    Variant2Type arg1Type arg2Type ->
                        Variant2Value
                            (forGadgetWithConfigHelp config_ arg1Type)
                            (forGadgetWithConfigHelp config_ arg2Type)

                    Variant3Type arg1Type arg2Type arg3Type ->
                        Variant3Value
                            (forGadgetWithConfigHelp config_ arg1Type)
                            (forGadgetWithConfigHelp config_ arg2Type)
                            (forGadgetWithConfigHelp config_ arg3Type)

                    Variant4Type arg1Type arg2Type arg3Type arg4Type ->
                        Variant4Value
                            (forGadgetWithConfigHelp config_ arg1Type)
                            (forGadgetWithConfigHelp config_ arg2Type)
                            (forGadgetWithConfigHelp config_ arg3Type)
                            (forGadgetWithConfigHelp config_ arg4Type)

                    Variant5Type arg1Type arg2Type arg3Type arg4Type arg5Type ->
                        Variant5Value
                            (forGadgetWithConfigHelp config_ arg1Type)
                            (forGadgetWithConfigHelp config_ arg2Type)
                            (forGadgetWithConfigHelp config_ arg3Type)
                            (forGadgetWithConfigHelp config_ arg4Type)
                            (forGadgetWithConfigHelp config_ arg5Type)
                )

        RecordType _ namedFieldTypes ->
            RecordValue
                (List.map
                    (\( fieldName, fieldType ) ->
                        ( fieldName, forGadgetWithConfigHelp config_ fieldType )
                    )
                    namedFieldTypes
                )

        ListType _ _ ->
            ListValue []

        PairType _ a b ->
            PairValue
                (forGadgetWithConfigHelp config_ a)
                (forGadgetWithConfigHelp config_ b)

        TripleType _ a b c ->
            TripleValue
                (forGadgetWithConfigHelp config_ a)
                (forGadgetWithConfigHelp config_ b)
                (forGadgetWithConfigHelp config_ c)

        LazyType _ thunk ->
            forGadgetWithConfigHelp config_ (thunk ())
