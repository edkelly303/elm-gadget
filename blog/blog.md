Friends, have you ever been in this situation?

Gotta write my application, gonna make some types...

```elm
type alias User =
    { name : String
    , isCool : Bool
    }
```

Mmm, need to load these users from the backend, better write a JSON decoder...

```elm
import Json.Decode as JD -- elm/json
import Json.Decode.Pipeline as JDP -- NoRedInk/elm-json-decode-pipeline

userDecoder = 
    JDP.succeed User
        |> JDP.required "name" JD.string
        |> JDP.required "isCool" JD.bool
```

Mmm, want to do some fuzz testing, better write a fuzzer...

```elm
import Fuzz as F -- elm-explorations/test

userFuzzer =
    F.constant User
        |> F.andMap F.string
        |> F.andMap F.bool
```

Mmm, want to generate some random users for my test environment, better write a
generator...

```elm
import Random as R -- elm/random
import Random.Extra as RE -- elm-community/random-extra
import Random.String as RS -- elm-community/random-extra
import Random.Char as RC -- elm-community/random-extra

userGenerator =
    R.constant User
        |> RE.andMap (RS.string 5 RC.unicode)
        |> RE.andMap RE.bool
```

I'm getting deja-vu at this point. It seems like I'm kinda doing the same thing over and over again. All these packages (if you squint a bit) seem to have the same API.

"Yes, that's an applicative"

Thank you Mr Haskell guy! But hang on a second, it goes deeper than that.

We've got a JSON decoder, but let's face it, we probably want an encoder too...

```elm
import Json.Encode as JE -- elm/json

encodeUser user =
    JE.object
        [ ( "name", JE.string user.name )
        , ( "isCool", JE.bool user.isCool )
        ]
```

With a separate encoder and decoder, there's always a risk that things could get out of sync. What if we followed the example of that clever fellow @miniBill and used a _codec_ instead?

```elm
import Codec as C -- miniBill/elm-codec

userCodec =
    C.object User
        |> C.field "name" .name C.string
        |> C.field "isCool" .isCool C.bool
        |> C.buildObject
```

This is cool because we get an encoder and a decoder in one go. And the API is only a _tiny_ bit more complicated than the applicative one. Notice that with each call of `C.field`, we need to add a getter (`.name` and `.isCool`), and that at the end we need to pipe into a final function (`C.buildObject`).

But although this is way better, it only helps us with the case of JSON encoding and decoding. We still need separate boilerplate for the fuzzer and generator. And if we wanted to add other functionality to our type (say, a function that could print it as a nicely formatted string, or a parser that can turn a string into a `User` value), that would mean yet more boilerplate.

What if we could generalise a bit more?

Check this out:

```elm
import Gadget -- edkelly303/elm-gadget

userGadget =
    Gadget.record User
        |> Gadget.field "name" .name Gadget.string
        |> Gadget.field "isCool" .isCool Gadget.bool
        |> Gadget.endRecord
```

Ok, so what does this `userGadget` thing do? By itself, nothing very useful. It effectively teaches Elm how a `User` value is structured by converting it into an "intermediate representation" (IR):

```elm
import Gadget.IR -- edkelly303/elm-gadget

userIR = 
    Gadget.IR.fromInput userGadget { name = "Ed", isCool = False }

userIR --> Gadget.IR.RecordValue 
       --      [ ( "name", Gadget.IR.StringValue "Ed" )
       --      , ( "isCool", Gadget.IR.BoolValue False )
       --      ]
```

We can also attempt to convert the IR back into a value of the original type:

```elm
import Gadget.IR -- edkelly303/elm-gadget

userResult = 
    Gadget.IR.toOutput userGadget userIR

userResult --> Ok { name = "Ed", isCool = False }
```

Now for the cunning part: once we have the IR, we can write functions that do interesting things with it. I call these functions "Adapters".

```elm
import Gadget.IR -- edkelly303/elm-gadget
import Json.Encode as JE -- elm/json

jsonEncodeAdapter gadget value =
    let 
        encodeIR ir = 
            case ir of
                Gadget.IR.StringValue s ->
                    JE.string s
                
                Gadget.IR.BoolValue b ->
                    JE.bool b
                
                Gadget.IR.RecordValue namedFields ->
                    JE.object
                        (List.map 
                            (\( name, field ) ->
                                ( name, encodeIR field )
                            ) 
                            namedFields
                        )
                
                _ -> 
                    Debug.todo "handle other cases"
    in
    value
        |> IR.fromInput gadget 
        |> encodeIR
```

And then:

```elm
import Json.Encode as JE -- elm/json

encodeUser = 
    jsonEncodeAdapter userGadget

json = 
    JE.encode 0 (encodeUser { name = "Ed", isCool = False })

json --> """{"name":"Ed","isCool":false}"""
```

Using this and a few other similar tricks, we can create Adapters that do all sorts of things: create fuzzers, generators, parsers, pretty-printers, differs and patchers, encoders and decoders. 

Once you have written a Gadget for your type, you can use it with any of these Adapters to do whatever you want in just a couple of lines of code. So, no more defining individual fuzzers, generators etc. for every type in your codebase. Just define a Gadget once and you're all set.

## Does this really work for all Elm types?

Well, no. It doesn't work for function types, or for types that contain functions. 

But it totally works for all the primitive types (`Int`, `Float`, `Char`, `String`, `Bool`), all the product types (records, tuples, triples), and for custom types, including recursive types like `List`.

Looking at the APIs for custom types vs record types reveals something that I find quite beatiful:

```elm
type alias Record = 
    { a : Int
    , b : Float 
    }

type Custom 
    = A Int
    | B Float

recordGadget = 
    Gadget.record 
        (\a b ->
            { a = a
            , b = b
            }
        )
        |> Gadget.field "a" .a Gadget.int
        |> Gadget.field "b" .b Gadget.float
        |> Gadget.endRecord

customGadget =
    Gadget.custom 
        (\a b variant -> 
            case variant of
                A int -> a int
                B float -> b float
        )
        |> Gadget.variant1 "A" A Gadget.int
        |> Gadget.variant1 "B" B Gadget.float
        |> Gadget.endCustom
```

Do you see it? 

`Gadget.record` and `Gadget.custom` each take a function as an argument. For `Gadget.record`, it's a _constructor_ - a function that tells Elm how to build a value of type `Record` out of the fields provided. For `Gadget.custom`, it's a _destructor_ - a function that tells Elm how to extract data out of an existing `Custom` value.

Now, look at `Gadget.field` and `Gadget.variant1`. This time, it's the opposite way around. `Gadget.field` takes `.a`, which is a _destructor_ - it tells Elm how to get the value out of the `a` field of the record. Whereas `Gadget.variant1` takes `A`, which is a _constructor_, telling Elm how to build a value of type `Custom` when given an `Int`.

Credit once again to @miniBill for discovering these APIs in his work on `elm-codec`. I think he would agree that this is a case of "discovered" rather than "invented" - the duality between the custom (sum) and record (product) types has a kind of inevitability to it. (Indeed, when I blunderingly tried to find a way to do custom types in my `elm-any-type-forms` package, before I'd seen `elm-codec`, after many failed attempts I eventually converged on exactly the same API.)

## Where can I get one of these Gadget thingies?

I haven't published this on the Elm package registry yet, but you can check out the repo at https://github.com/edkelly303/elm-gadget. It already includes some example Adapters for JSON encoding and decoding, fuzzing, generating random values, converting values to strings and parsing them, diffing and patching values, pretty-printing them, and displaying them as HTML. Some of these examples also show how it's possible to attach metadata to Gadgets, which gives you a way to reconfigure or override the default behaviour of an Adapter.

Why haven't I published it? I have a history of publishing highly experimental, unfinished packages that I subsequently want to disavow. This time, I want to be sure I'm publishing something that I can be proud of. Before I can get there, I still have a few open design questions that I want to address, especially around how best to attach metadata to Gadgets. There are also some more types of Adapters that I'd like to experiment with to make sure they're actually possible.

What I would like, though, is if some of you want to experiment with the package in its current state and see what kinds of interesting Adapters you can come up with. For example:
* Could we do forms? (I'm pretty certain we could)
* Could we do something self-referential like printing out a Gadget's own definition? (I'm not sure) 
* What's _not_ possible, that could or should be possible?