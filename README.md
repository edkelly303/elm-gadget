# elm-gadget

## What is this?

A Gadget is an encoder/decoder (codec) that converts between Elm data types and
a generic intermediate representation (IR).

## Why is that useful?

It kinda depends who you are.

**For application developers:** Once you've defined a Gadget for your data
types, you can use it with a wide range of adapters to get useful functionality
for free. Instead of writing a JSON encoder and decoder, plus a fuzzer, plus a
random generator, etc. for each data type, you just define a Gadget once and
you're all done.

* Check out the [`Gadget`](https://package.elm-lang.org/packages/edkelly303/elm-gadget/1.0.0/Gadget) module to learn how to define Gadgets for
  your data types.

* Check out the `Gadget.Adapter` modules to start using your Gadgets to do cool
  stuff!

**For tooling authors:** The ability to convert any Elm data type to IR makes it
relatively easy to build cool tools, such as JSON encoders/decoders, fuzzers,
random generators, diff/patchers, parser/printers, and any other bidirectional
converters.

* Check out the [`Gadget.IR`](https://package.elm-lang.org/packages/edkelly303/elm-gadget/1.0.0/Gadget-IR) module docs for the types and functions
  you need to write your own adapters.

* Check out the source code of the `Gadget.Adapter` modules for some examples of
  how to write adapters.

## Show me some examples!

Do `npx run-pty run-pty.json` in the project root folder.

## What does the code look like?

```elm
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Json
import Fuzz
import Json.Decode
import Json.Encode

-- For a data type like this:

type alias Person = 
    { name : String
    , age : Int 
    }

input = 
    { name = "Ed"
    , age = 44 
    }

-- Just write a Gadget like this:

gadget : Gadget.Gadget Person
gadget = 
    Gadget.record Person
        |> Gadget.field .name Gadget.string 
        |> Gadget.field .age Gadget.int
        |> Gadget.endRecord

-- Want to turn your data into JSON? Just use 
-- this handy adapter I wrote!

json = 
    Gadget.Adapter.Json.encode gadget input

json --: Json.Encode.Value

-- And now let's get it back again from JSON:

decoded = 
    Json.Decode.decodeValue 
        (Gadget.Adapter.Json.decoder gadget) 
        json

decoded --> Ok { name = "Ed", age = 44 }

-- Now, say we also want a fuzzer 
-- for testing: we can turn the same
-- Gadget into a fuzzer too!

fuzzed = 
    Fuzz.examples 1 (Gadget.Adapter.Fuzz.fuzzer gadget)

fuzzed --> [ { age = 92, name = "o \n\\" } ]
```
