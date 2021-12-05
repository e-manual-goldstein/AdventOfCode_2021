module Utils
open System

let readLines filePath = System.IO.File.ReadLines(filePath);;

let rec Sum(integers:list<int>) =
    if integers.IsEmpty then 0
    else if integers.Length = 1 then integers.Head
    else integers.Head + Sum(integers.Tail)

let FlipBinaryValue(singleValue:string) =
    if String.Equals(singleValue, "1") then "0"
    else if String.Equals(singleValue, "0") then "1"
    else String.Empty

let rec InvertBinary(binaryString:string) =
    if binaryString.Length = 0 then String.Empty
    else FlipBinaryValue(binaryString.Substring(0,1)) + InvertBinary(binaryString.Substring(1))

let ConvertBinaryDigitToDecimal(binaryString:string, index:int) =
    if binaryString.Length <= index then raise (ArgumentOutOfRangeException())
    else (binaryString.Substring(index,1) |> float) * ((2 |> float) ** (binaryString.Length - index - 1 |> float))
    

let rec ConvertBinaryStringToDecimal(binaryString:string, index:int) =
    if binaryString.Length <= index then 0 |> float
    else ConvertBinaryDigitToDecimal(binaryString, index) + ConvertBinaryStringToDecimal(binaryString, index + 1)

let IntegerSeqFromFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () |> int
}

let StringSeqFromFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseIntegerListFromFile (filePath:string) = 
    IntegerSeqFromFile filePath |> List.ofSeq

let parseStringListFromFile (filePath:string) = 
    StringSeqFromFile filePath |> List.ofSeq

let rec createCsv (stringList:list<'T>, output:string) =
    if stringList.IsEmpty then output
    else if stringList.Length = 1 then $"{stringList.Head}"
    else $"{stringList.Head}, " + createCsv (stringList.Tail, $"{output}")

let printValuesInLine (entries:list<'T>) =
    let csv = createCsv(entries, "")
    printfn "values = '%s'" csv

let rec printValuesInSequence (entries:list<'T>) = 
    if entries.IsEmpty then ()
    else 
        printfn $"{entries.Head}"
        printValuesInSequence entries.Tail