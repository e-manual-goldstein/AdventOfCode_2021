

open System
open AOC.Day1
open Utils


[<EntryPoint>]
let main argv =
    //let input = parseIntegerArrayFromFile(@"testinput.txt")
    let sonarReadings = [1; 2; 3; 4; 5]
    let output = AOC.Day1.SumOfNextNTerms (sonarReadings, 4, 0);
    printfn "output is %i" output
    0 // return an integer exit code
