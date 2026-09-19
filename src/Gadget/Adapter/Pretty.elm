module Gadget.Adapter.Pretty exposing (print)

{-|


## ☠️ **Warning:** Not designed for production use! ☠️

The `Gadget.Adapter` modules are included in this package as toy adapters to
show you what Gadgets are capable of, and provide source-code examples that you
can use to get started with writing your own adapters. You should probably write
your own production-grade adapters that are designed for your specific use-case.


## Introduction

Use a Gadget to convert Elm values into a pretty-printed `String`, using
[`the-sett/elm-pretty-printer`](https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/).


## API

@docs print

-}

import Gadget.Adapter.Pretty.Elm as Elm
import Gadget.IR as IR
import Lib.Glam as G


{-| Print an Elm value as a `String`, wrapped prettily at a given number of
columns
-}
print : IR.Gadget a -> Int -> a -> String
print gadget width input =
    IR.fromInput gadget input
        |> Elm.toDocument
        |> G.group
        |> G.toString width
