module Utils

let readLines filePath = System.IO.File.ReadLines(filePath);;



let IntegerSeqFromFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () |> int
}

let parseIntegerArrayFromFile (filePath:string) = 
    IntegerSeqFromFile filePath |> List.ofSeq

let rec createCsv (stringList:list<'T>, output:string) =
    //printfn $"{stringList.Head}"
    if stringList.IsEmpty then output
    else if stringList.Length = 1 then $"{stringList.Head}"
    else $"{stringList.Head}, " + createCsv (stringList.Tail, $"{output}")

let printValues (entries:list<'T>) =
    let csv = createCsv(entries, "")
    printfn "values = '%s'" csv