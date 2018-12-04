open System
open Util.Regex

type GuardSchedule() =
    [<DefaultValue>] val mutable Id : int
    [<DefaultValue>] val mutable AsleepTimesByDay : Map<int*int,bool []> // tuple of (month,day) to array (length 60) of minutes guard is asleep

let split (input:string)=
    input.Split("\r\n")

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

    let guards = List.empty

    let gs = new GuardSchedule()
    gs.Id <- 0

    for line in (split testinput) do
        match line with
        | Regex @"\[\d+-(\d+)-(\d+) \d+:(\d+)\] (.+)" [month; day; minute; action;] ->
            match action with
            | Regex @".+#(\d).+" [id] ->
                if gs.Id <>0 then

            |
        | _ -> printfn "error parsing line %s" line

    0 // return an integer exit code
