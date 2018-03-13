module Number.Bounded
    exposing
        ( Bounded
        , between
        , set
        , inc
        , dec
        , minBound
        , maxBound
        , value
        )

{-| A type representing bounded numbers. Once a bound is set, the bounded value can never go out of this range.

    between 1 10
        |> set 7
        |> inc 5
        |> value
    -- (equals 10)

@docs Bounded

@docs between, set, inc, dec

@docs value, minBound, maxBound

-}


{-| -}
type Bounded a
    = Bounded { min : a, max : a, value : a }


{-| Check if a number is negative, only using `number` types
We can't use `<` since it takes `comparable`
And `/` takes `Float`, not `Int`
However `abs`, `*` and `^` all operate on `number`
-}
isNegative : number -> Bool
isNegative x =
    x * ((abs x) ^ -1) == -1


{-| Minimum function defined over 'number' types rather than 'comparable' types
-}
minimum : number -> number -> number
minimum a b =
    if isNegative (a - b) then
        a
    else
        b


{-| Maximum function defined over 'number' types rather than 'comparable' types
-}
maximum : number -> number -> number
maximum a b =
    if isNegative (a - b) then
        b
    else
        a


{-| Initialize a bounded number by giving it a min and max for the bounds (inclusive). The value will be initialized as the provided min. The min will always be the lower number, regardless of which order you provide the arguments.
-}
between : comparable -> comparable -> Bounded comparable
between a b =
    if a < b then
        Bounded { min = a, max = b, value = a }
    else
        Bounded { min = b, max = a, value = b }


{-| Set the value manually. If you try to set a value greater than the max bound, it will "clip" at the max. Likewise, if you try to set a value less than the min bound, it will clip at the min.
-}
set : comparable -> Bounded comparable -> Bounded comparable
set value (Bounded { min, max }) =
    Bounded { min = min, max = max, value = Basics.max min <| Basics.min max value }


{-| Increments the value by the given amount, "clipping" at the max bound if it passes it.
-}
inc : number -> Bounded number -> Bounded number
inc by (Bounded { min, max, value }) =
    Bounded { min = min, max = max, value = minimum max <| value + by }


{-| Decrements the value by the given amount, "clipping" at the min bound if it passes it.
-}
dec : number -> Bounded number -> Bounded number
dec by (Bounded { min, max, value }) =
    Bounded { min = min, max = max, value = maximum min <| value - by }


{-| Get the value
-}
value : Bounded a -> a
value (Bounded { value }) =
    value


{-| Get the min bound
-}
minBound : Bounded a -> a
minBound (Bounded { min }) =
    min


{-| Get the max bound
-}
maxBound : Bounded a -> a
maxBound (Bounded { max }) =
    max
