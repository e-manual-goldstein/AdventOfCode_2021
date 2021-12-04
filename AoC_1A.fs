
module AOC.Day1

open System

let sonarReadings = [1; 2; 3; 4; 5]


let rec Sum(readings:list<int>, previousValue:int) = 
    if not readings.IsEmpty then Sum (readings.Tail, readings.Head + previousValue)
    else previousValue

let rec CountIncreases(readings:list<int>, previousValue:Nullable<int>) =
    if readings.IsEmpty then 0 // no readings provided
    else if not previousValue.HasValue then CountIncreases(readings.Tail, Nullable(readings.Head)) // first entry in list
    else if previousValue.Value < readings.Head then CountIncreases(readings.Tail, Nullable(readings.Head)) + 1// nth item in list (n > 1)
    else CountIncreases(readings.Tail, Nullable(readings.Head))

let rec SumOfNextNTerms(entries:list<int>, n:int, total:int) =
    printfn "'n' is '%i'" n
    if n < 0 then raise (System.ArgumentException("'n' must be positive"))
    else if n = 0 then total
    else SumOfNextNTerms(entries.Tail, n - 1, total) + entries.Head;

//let rec CountIncreasesInSlidingWindow(readings:list<int>, previousValue:Nullable<int>) =
