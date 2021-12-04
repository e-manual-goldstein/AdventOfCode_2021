module LearningModule
// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let logEntryMonday = "first entry"
let logEntryTuesDay = "second entry"
let logEntryWednesday = "third entry"

let logEntriesWeek = ["first entry"; "second entry"; "third entry"]

let cards = ["Ace"; "King"; "Queen"]

let newList = "Jack" :: cards // "Jack", "Ace", "King", "Queen" 

let otherCardList = ["Jack"; "10"]
let fullList = cards @ otherCardList // "Ace", "King", "Queen", "Jack", "10"

let fullList2 = cards |> List.append ["Jack"]
let fullList3 = cards |> List.append otherCardList // "Ace", "King", "Queen", "Jack", "10", "9"

let drawCard (list:string list) = 
    printfn "%s" list.Head
    list.Tail 
