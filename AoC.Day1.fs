
module AoC.Day1

open System
open Utils

let sonarReadings = [199;200;208;210;200;207;240;269;260;263]
let puzzleInput = parseIntegerArrayFromFile(@"Day1_Input.txt")


let rec Sum(readings:list<int>, previousValue:int) = 
    if not readings.IsEmpty then Sum (readings.Tail, readings.Head + previousValue)
    else previousValue

let rec CountIncreases(readings:list<int>, previousValue:Nullable<int>) =
    if readings.IsEmpty then 0 // no readings provided
    else if not previousValue.HasValue then CountIncreases(readings.Tail, Nullable(readings.Head)) // first entry in list
    else if previousValue.Value < readings.Head then CountIncreases(readings.Tail, Nullable(readings.Head)) + 1// nth item in list (n > 1)
    else CountIncreases(readings.Tail, Nullable(readings.Head))

let rec SumOfNextNTerms(entries:list<int>, n:int, total:int) =
    // printfn "'n' is '%i'" n
    // printfn "'total' is '%i'" total
    if n < 0 then raise (System.ArgumentException("'n' must be positive"))
    else if n > entries.Length then raise (System.ArgumentOutOfRangeException("'n' cannot exceed list length"))
    else if n = 0 then total
    else SumOfNextNTerms(entries.Tail, n - 1, total + entries.Head);

let rec GetSlidingWindowValues(sourceList:list<int>, targetList:list<int>, windowSize:int) = 
    if sourceList.Length < windowSize then targetList // if there aren't enough numbers, then return whatever is part of the list
    else GetSlidingWindowValues(sourceList.Tail, targetList @ [SumOfNextNTerms(sourceList, windowSize, 0)], windowSize)
    
let firstStar () =
    let output = CountIncreases(puzzleInput, Nullable()) 
    printfn "output is %i" output
    
let secondStar () =
    let windowValues = GetSlidingWindowValues(puzzleInput, list.Empty, 3)
    let output = CountIncreases(windowValues, Nullable());
    printfn "output is %i" output