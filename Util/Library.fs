namespace Util

open System.Text.RegularExpressions

module Base =
    open System.IO

    let parse (input:string) parseLine =
        input.Split("\r\n")
        |> Array.map parseLine

    let readLines (filePath:string) = seq{
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine
    }

module Say =
    let hello name =
        printfn "Hello %s" name

module Arr =
    let tee a =
        printfn "%A" a
        a

module Regex =
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
