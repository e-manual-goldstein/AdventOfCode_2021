
module AoC.Day3

open System
open Utils

let testInputs = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]
let puzzleInput = parseStringListFromFile(@"Day3_Input.txt")

let GetBitValueAtIndex(inputString:string, index) = 
    inputString.ToCharArray()[index] |> string |> int

let rec SumValuesAtIndex (inputList:list<string>, index:int) =
    if inputList.IsEmpty then 0
    //else if diagnosticOutput.Length = 1 then GetBitValueAtIndex(diagnosticOutput.Head, index)
    else GetBitValueAtIndex(inputList.Head, index) + SumValuesAtIndex (inputList.Tail, index)

let CalculateBitCriteriaAtIndex (inputList:list<string>, index:int, bitCriteriaType:string) = 
    let sum = SumValuesAtIndex(inputList, index)
    if String.Equals(bitCriteriaType, "Most") then 
        if inputList.Length - sum > sum then "0"
        else "1"
    else if String.Equals(bitCriteriaType, "Least") then 
        if inputList.Length - sum > sum then "1"
        else "0"
    else raise (ArgumentException("Unknown Bit Criteria Type"))


let CalculateGammaAtIndex (inputList:list<string>, index:int) = 
    let sum = SumValuesAtIndex(inputList, index)
    if inputList.Length - sum > sum then 0
    else 1

let rec CalculateGammaForOutput(diagnosticOutput:list<string>, index:int) = 
    if diagnosticOutput.Head.Length <= index then String.Empty
    else $"{CalculateGammaAtIndex(diagnosticOutput, index)}" + CalculateGammaForOutput(diagnosticOutput, index + 1)

let CalculatePowerConsumption (diagnosticOutput:list<string>) =
    let gamma = CalculateGammaForOutput(diagnosticOutput, 0)
    let epsilon = InvertBinary(gamma)
    ConvertBinaryStringToDecimal(gamma, 0) * ConvertBinaryStringToDecimal(epsilon, 0) |> int


let rec FilterByClosestMatch (inputList:list<string>, index:int, bitCriteriaType:string) =
    if inputList.Length = 1 then inputList[0]
    else 
        let bitCriteria = CalculateBitCriteriaAtIndex(inputList, index, bitCriteriaType)
        let filteredList = query {
            for reading in inputList do
            where (String.Equals(reading.Substring(index,1), bitCriteria))
            select reading
        }
        FilterByClosestMatch(filteredList |> List.ofSeq, index + 1, bitCriteriaType)
    

let CalculateOxygenGeneratorRating (diagnosticOutput:list<string>) =
    let oxygenReading = FilterByClosestMatch (diagnosticOutput, 0, "Most")
    ConvertBinaryStringToDecimal(oxygenReading, 0) |> int

let CalculateCO2ScrubberRating (diagnosticOutput:list<string>) =
    let co2ScrubberReading = FilterByClosestMatch (diagnosticOutput, 0, "Least")
    ConvertBinaryStringToDecimal(co2ScrubberReading, 0) |> int

let LifeSupportRating (diagnosticOutput:list<string>) =
    CalculateOxygenGeneratorRating(diagnosticOutput) * CalculateCO2ScrubberRating(diagnosticOutput)

let firstStar () =
    let output = CalculatePowerConsumption(puzzleInput)
    printfn "output is %i" output
    
let secondStar () =
    let output = LifeSupportRating puzzleInput
    printfn "output is %i" output