
module AoC.Day3

open System
open Utils

let controls = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]
let puzzleInput = parseStringListFromFile(@"Day3_Input.txt")



let ParseControl (control:string) = 
    control.Split [|' '|]

let rec ParseControls (controls:list<string>) = 
    if controls.IsEmpty then list.Empty
    else ParseControl controls.Head :: ParseControls(controls.Tail)

let ExecuteBasicInput (input:string, horizonalPosition:int, depth:int) =
    let control = ParseControl input
    let direction = control[0]
    let magnitude = control[1] |> int
    if String.Equals(direction, "forward") then (horizonalPosition + magnitude, depth)
    else if String.Equals(direction, "up") then (horizonalPosition, depth - magnitude)
    else if (String.Equals(direction, "down")) then (horizonalPosition, depth + magnitude)
    else raise (ArgumentException("Unknown Control"))

let ExecuteComplexInput (input:string, horizonalPosition:int, depth:int, aim:int) =
    let control = ParseControl input
    let direction = control[0]
    let magnitude = control[1] |> int
    if String.Equals(direction, "forward") then (horizonalPosition + magnitude, depth + (aim * magnitude), aim)
    else if String.Equals(direction, "up") then (horizonalPosition, depth, aim - magnitude)
    else if (String.Equals(direction, "down")) then (horizonalPosition, depth, aim + magnitude)
    else raise (ArgumentException("Unknown Control"))


let rec GetNewPositionBasic(horizonalPosition:int, depth:int, commands:list<string>) = 
    let newPosition = ExecuteBasicInput(commands.Head, horizonalPosition, depth)
    let newHorizontalPosition = fst newPosition
    let newDepth = snd newPosition
    if commands.Tail.IsEmpty then (newHorizontalPosition * newDepth)
    else GetNewPositionBasic(newHorizontalPosition, newDepth, commands.Tail)

let rec GetNewPositionComplex(horizonalPosition:int, depth:int, aim:int, commands:list<string>) = 
    let (newHorizontalPosition, newDepth, newAim) = ExecuteComplexInput(commands.Head, horizonalPosition, depth, aim)
    if commands.Tail.IsEmpty then (newHorizontalPosition * newDepth)
    else GetNewPositionComplex(newHorizontalPosition, newDepth, newAim, commands.Tail)


let firstStar () =
    let output = GetNewPositionBasic(0,0, puzzleInput)
    printfn "output is %i" output
    
let secondStar () =
    let output = GetNewPositionComplex(0,0, 0, puzzleInput)
    printfn "output is %i" output