open System
open Util.Base
open System.IO
open Util.Arr

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
    reactAt 0 (Array.copy polymer)

let bestRemovalResult parsed =
    [for a in 'a'..'z' do yield (a,Char.ToUpper a)]
    |> List.map (fun (lower,upper) ->
        let filtered =
            parsed
            |> Array.filter (fun element -> element <> lower && element <> upper)
        if filtered.Length = parsed.Length // if the element didn't occur in the polymer, ignore it in the calculation
        then 0
        else
            filtered
            |> react
            |> Array.length
    )
    |> List.filter ((<>)0)
    |> List.min

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

    stopwatch.Restart()

    let res2 = bestRemovalResult parsed

    stopwatch.Stop()
    printfn "the result of day 5 part 2 is: %i" res2
    printfn "\t the calculation took %i milliseconds" stopwatch.ElapsedMilliseconds
    Console.ReadKey() |> ignore

    0 // return an integer exit code
