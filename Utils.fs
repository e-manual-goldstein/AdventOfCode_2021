module Utils

let readLines filePath = System.IO.File.ReadLines(filePath);;

let rec Sum(integers:list<int>) =
    if integers.IsEmpty then 0
    else if integers.Length = 1 then integers.Head
    else integers.Head + Sum(integers.Tail)

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