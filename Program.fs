

open System
open AOC.Day1
open Utils


[<EntryPoint>]
let main argv =
    let sonarReadings = parseIntegerArrayFromFile(@"testinput.txt")
    //let sonarReadings = [199;200;208;210;200;207;240;269;260;263]
    printValues sonarReadings
    let windowValues = GetSlidingWindowValues(sonarReadings, list.Empty, 3)
    printValues windowValues
    let output = CountIncreases(windowValues, Nullable());
    printfn "output is %i" output
    0 // return an integer exit code
