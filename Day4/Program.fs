open System
open Util.Regex

let split (input:string)=
    input.Split("\r\n")

let rec handle guard (datastructure:Map<string,Map<string,bool []>>) arr =
    match arr with
    | head :: tail ->
        match head with
        | Regex @"\[.+?-(\d\d\-\d\d) \d+:(\d+)\] (.+)" [day; min; action] ->
            match action with
            | Regex @"Guard #(\d+)" [newguard] ->
                if datastructure.ContainsKey newguard then
                    handle newguard datastructure tail
                else
                    // create new level in datastructure
                    // new array of waking minutes, default to sleep
                    let minutes : bool array = Array.zeroCreate 60

                    // guard starts out as awake
                    minutes.[min|>int] <- true
                    let newMap =
                        Map.empty
                        |> Map.add day minutes
                    let newDs =
                        datastructure
                        |> Map.add newguard newMap
                    handle newguard newDs tail
            | Regex @"wakes up" [] ->

            | Regex @"falls asleep" []->

            | _ -> handle guard datastructure tail
        | _ ->
    | [] -> datastructure

[<EntryPoint>]
let main argv =
    let testinput ="[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"

    // (guard,day,minute) -> awake

    let split = split testinput

    split
    |>

    0 // return an integer exit code
