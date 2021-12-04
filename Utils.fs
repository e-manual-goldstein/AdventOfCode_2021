module Utils

let readLines filePath = System.IO.File.ReadLines(filePath);;



let IntegerSeqFromFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () |> int
}

let parseIntegerArrayFromFile (filePath:string) = 
    IntegerSeqFromFile filePath |> List.ofSeq