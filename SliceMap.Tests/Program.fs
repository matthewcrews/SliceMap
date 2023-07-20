open SliceMap
open Xunit

module Program =
    let [<EntryPoint>] main _ =
        #if FABLE_COMPILER
        // putting a sample here
        let x = SMap.ofList [for i in 1..5 -> i, i]
        let isDivisibleBy2 x = x % 2 = 0
        let ok = x.[Where isDivisibleBy2] =  SMap [(2, 2); (4, 4)]
        if not ok then
            failwith $"%A{x.[Where isDivisibleBy2]}\nexpected\n%A{SMap [(2, 2); (4, 4)]}"
        else
            printfn "ok!"
            // python: fails on TryFind.toMap due to Comparer
            printfn $"{x.[Where isDivisibleBy2]} looks ok :)"
        #endif
        0
