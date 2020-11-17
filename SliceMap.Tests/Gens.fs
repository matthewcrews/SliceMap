namespace SliceMap.Tests

module Gens =

    open FsCheck

    type SmallFloat = SmallFloat of float

    let MIN_FLOAT = -1e18
    let MAX_FLOAT = 1e18
    let MIN_DISTANCE_FROM_ZERO = 1e-9

    let FloatGen = Arb.generate<float>.Where(fun x -> x > MIN_FLOAT && x < MAX_FLOAT && (x > (MIN_DISTANCE_FROM_ZERO) || x < (-1.0 * MIN_DISTANCE_FROM_ZERO)))

    let PositiveFloatGen = Arb.generate<float>.Where(fun x -> x > 0.0 && x < MAX_FLOAT)

    let SmallFloatGen =
        gen {
            let! f = FloatGen
            return SmallFloat f
        }

    let ScalarGen =
        gen {
            let! f = FloatGen
            return Value f
        }


    type Types () =
        static member ArbSmallFloatGen () = Arb.fromGen SmallFloatGen
        static member ArbScalarGen () = Arb.fromGen ScalarGen

            