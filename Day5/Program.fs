open System
open Util.Base
open System.IO

let parse (input:string) =
    input.ToCharArray()

let reactable (a:char) (b:char) =
    abs ((int a) - (int b)) = 32

let rec reactAt index (polymer:char array) =
    if (index+1) < polymer.Length then
        if reactable polymer.[index] polymer.[index+1] then
            polymer.[index] <- '.'
            polymer.[index+1] <- '.'
            reactAt (if (index-1) > 0 then (index - 1) else 0) (Array.filter ((<>)'.') polymer)
        else
            reactAt (index+1) polymer
    else
        polymer

let react (polymer:char array) =
    reactAt 0 polymer

[<EntryPoint>]
let main argv =
    let testinput = "dabAcCaCBAcCcaDA"

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let filePath = "day5.txt"
    let input = File.ReadLines(filePath) |> Seq.head

    let parsed =
        parse input

    stopwatch.Restart()
    let res = react parsed
    stopwatch.Stop()

    printfn "the result of day 5 part 1 is: %i" res.Length
    printfn "\t the calculation took %i milliseconds" stopwatch.ElapsedMilliseconds
    Console.ReadKey() |> ignore

    0 // return an integer exit code
