open System
open Util.Arr

let parse (input:string) =
    input.Split("\r\n")
    |> Seq.map (fun word -> Seq.toList(word))
    |> Seq.toList

let countChar (a:char) (word:string) =
    Seq.fold (fun acc i -> if a.Equals(i) then acc + 1 else acc) 0 word

let rec createMap (counts:Map<char,int>) word =
    match word with
    | head :: tail ->
        if Map.containsKey head counts
        then
            createMap (counts.Add(head, (counts.[head]+1))) tail
        else
            createMap (counts.Add(head, 1)) tail
    | [] -> counts

let countTwosAndThrees allCharMap =
    ((Map.filter (fun _ v -> v = 2) allCharMap).Count,(Map.filter (fun _ v -> v = 3) allCharMap).Count)

let normalize a =
    match a with
    | 0 -> 0
    | _ -> 1

let differsByOneChar (a:char list) (b:char list) =
    if a.Length <> b.Length
    then false
    else
        let eqs =
            List.zip a b
            |> List.filter (fun (x,y) -> x <> y)
        eqs.Length = 1

let getEqualChars (a:char list) (b:char list) =
    List.zip a b
    |> List.filter (fun (x,y) -> x=y)
    |> List.map (fun (x,y) -> x)

let filterForOneCharDiff (a:char list) (bs:char list list) =
    List.filter (fun b -> differsByOneChar a b) bs

[<EntryPoint>]
let main argv =
    let testinput = "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"

    let testinput2 ="abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz"

    let input ="umdryebvlapkozostecnihjexg
amdryebalapkozfstwcnrhjqxg
umdcyebvlapaozfstwcnihjqgg
ymdryrbvlapkozfstwcuihjqxg
umdrsebvlapkozxstwcnihjqig
umdryibvlapkohfstwcnfhjqxg
umdryebvqapkozfatwcnihjqxs
umzrpebvlapkozfshwcnihjqxg
fmhryebvlapkozfstwckihjqxg
umdryebvlahkozfstwcnizjrxg
qmdryebvlapkozfslwcnihgqxg
umdiyebjlapknzfstwcnihjqxg
umdryebvlapkoqfstwcaihvqxg
cmdryebvlapkpzfstwcnihjvxg
umdryebvlakkozfstwcgihjixg
umdryebvlasjozfstwcnihqqxg
umdryebvladkozfsvwcnifjqxg
umdrlebvlapaozfstwcniwjqxg
umdryebvlhpkozrstwsnihjqxg
umdryebvcapkozfqtwcnihjrxg
ubdrykbvlapkowfstwcnihjqxg
umdryebvldpkozfstwcnihtqsg
umdryebvlapaozyutwcnihjqxg
umdryibvlapkozfstdfnihjqxg
umdryebvlapgozkstwznihjqxg
umdrxebvlapkozfstwcngxjqxg
umdryekvlapkozfstwclchjqxg
nmdryebvlapkozjsewcnihjqxg
umdryebvyapkozfstfcniheqxg
umdfyebvlapkozfstwcnhhjpxg
umdryelylupkozfstwcnihjqxg
smdryebvlqpkozfstwcnihjdxg
umdryebvlapaozfsuwcnihjqxc
umdryebvlrzkozrstwcnihjqxg
umdbycbvlapkojfstwcnihjqxg
umdryebvlapkonfstwpnirjqxg
uecryebvlapkozfstwcnihpqxg
uqdryebvltpkozfstwcnihrqxg
umdryebvlqsknzfstwcnihjqxg
cmdryebvlapkocfstwcvihjqxg
umdrkebvlapkozqsfwcnihjqxg
umdryabveapkoifstwcnihjqxg
ummrnehvlapkozfstwcnihjqxg
umdryebvlxpkozfstwqnihjtxg
umdryebvlagkozastwcnihjqxh
umdryebvlatkozzhtwcnihjqxg
umdryebvlcpkozfstwrnihjqvg
umdryebvlapkozfsnwcnrhjcxg
umdzyebvlypkozfstwcnibjqxg
nmdryebvlvpkozbstwcnihjqxg
uwdryebvlipkozfstwcnihvqxg
umdraebvlavkozfstwcnihjqwg
umdeyebvlspbozfstwcnihjqxg
umdryxlvlapkozfstwcnihjqxu
umdryegvlapkqzfstwcnirjqxg
umdrupbvlapkozfstwcnihjqog
imxryebvlapkxzfstwcnihjqxg
umdrfebvlapkozowtwcnihjqxg
umdreebvlapkozmstwczihjqxg
undryebdlapkozbstwcnihjqxg
umdryebvlapkpzfetwcnihjqxb
ymdnyebvlapkozfstwinihjqxg
umdryebvaapkozfstwcnihyqqg
umdryebvlapkzzwsrwcnihjqxg
umdrkebvlapkmzfskwcnihjqxg
umdrmebvlapkozfsvwcnidjqxg
umdlyehvlapkozfstwcnihjqkg
umnryebvlrpkozfstwjnihjqxg
uqdryebvlapxozfsawcnihjqxg
vmdruebvlapkozfstwcnihjqqg
umdryabviapkozistwcnihjqxg
umdryebvlapkzzfstwfnihkqxg
uvdryebvlapkozfsxwcuihjqxg
umdlhebvlapkozfstwcnvhjqxg
umdreebvlapkopfstjcnihjqxg
umdryebvlazkomfstwynihjqxg
kmdryebulapkoznstwcnihjqxg
umdryebvxakkozfstwinihjqxg
ukdryobvlapkozistwcnihjqxg
umdryebveapkozfstwcnthjqgg
mmdrtebvlapcozfstwcnihjqxg
umdryebvlapkolistwnnihjqxg
umdryebxlapkozfatwcnihjqxx
uxdryebvlapkozfstwhniheqxg
ufdryebvzapkozfstwcnbhjqxg
amdryhbvlapkozfstwcnifjqxg
umqryebvlaphozfstwcnihjqxn
umdryebvlapkosfstfcnihjqxe
gmkryebvlapkozfstwcnihjmxg
umdrnebvlkpkozfstwcnihjnxg
umdryebvrapkozfstmcndhjqxg
umdryebvmapkozfstichihjqxg
umdryesvnapkozestwcnihjqxg
umeryhbvlapkozfstfcnihjqxg
umdryedvbapkozfstwcnihqqxg
umdryebllapzozfstwcnihjvxg
umdcyebvlzdkozfstwcnihjqxg
umdrybbvlapkbvfstwcnihjqxg
umdrytbglapkozfsthcnihjqxg
umdryebvlkpkozfsteclihjqxg
umdntebvlapkmzfstwcnihjqxg
lkdryebveapkozfstwcnihjqxg
ymdryubvlapkozfstwbnihjqxg
tmrryebvlapkozfstwcnqhjqxg
umdryeovlaekonfstwcnihjqxg
umiryeuvlapkozfstwcnihjwxg
umdryebvlspvozwstwcnihjqxg
umdrtebvlapkoznxtwcnihjqxg
umvryebvlaphozfstwcnahjqxg
umdryebvlapkozfstinniajqxg
umdryebqlapkozfctwcnihjqxx
umdryebvlapkbzfptwcnihjqvg
umdryabviapkozistwcnihjqxd
umdryrbvlapkezfstscnihjqxg
umhryebvlapkozfstacnihxqxg
umdxyelvlapkozfitwcnihjqxg
umdryevvuapkozfstwcnihtqxg
uydrypbvxapkozfstwcnihjqxg
umdryebvlapkopfstwcnihzqxo
uedryebvlapkozistwceihjqxg
umdiyebvlapkozfgtwcnihjqxv
ymdryebvlapkozfsticniqjqxg
umbrkebvlapkozfslwcnihjqxg
umdryebliapkozbstwcnihjqxg
umvryebolapkozfstwcnihjqig
umdryeavbackozfstwcnihjqxg
umdryfbvlapsozfstwcnihaqxg
umdqyebvlapkozfjtgcnihjqxg
umdrjebvlaqkozfstwcyihjqxg
umdryebklaqkozrstwcnihjqxg
umdryebvpapkozfstwcpihjqjg
uydryebhlawkozfstwcnihjqxg
umdyyebvlapkozfstwcykhjqxg
umdryebvlapkozfstwcnitjnxh
umdzyebvlapkozfstwcnehyqxg
mmcryebvlapkozfstwinihjqxg
umdryebvlapuozfstwmvihjqxg
umdryfbvlapkozqstwcnihjmxg
umdryebslapsozfhtwcnihjqxg
umdtyemvlapmozfstwcnihjqxg
umdrxevvlapkozfytwcnihjqxg
umdahebvlapjozfstwcnihjqxg
umdryebvlapkozfstacnivjqxb
umdryebvlzpkozfjtwcnihjyxg
umdryebvlaqkozfstwcnisjqxu
umdrydbvlapkozfsuwcnihjlxg
umdryebvlapkomrstwcnihjqkg
umdryebvlapcozfstmcnwhjqxg
umdryebvlahkozfstwcibhjqxg
gmdrzebvlapkozlstwcnihjqxg
umdryebvlapkezfsswcnrhjqxg
umdryebvlapkoqfitwcgihjqxg
umdrnebvlapkozfsiwcninjqxg
umdryebvlapkozfsrwckohjqxg
umdryebtlapkomfstwcnihjexg
umdryxbvlapjozfstwcnihoqxg
umdpyebvlapkosustwcnihjqxg
umdryebvlapkvzfawwcnihjqxg
umhnyebvlaikozfstwcnihjqxg
umdryebvlagkozfstvknihjqxg
uodryebjlapkoxfstwcnihjqxg
umdryefdlapkozfstwcnyhjqxg
umprmebvtapkozfstwcnihjqxg
umdhyebvlapoozfstwcnihjqgg
uddryebvidpkozfstwcnihjqxg
umdryebtlapkozfetwfnihjqxg
umdbyebolapkozfstwcoihjqxg
umdryebvlapkonfstwcnihjpxo
umdryebvlapkohfstwcnihjqwk
umdryebolalkkzfstwcnihjqxg
updryebvxapkozfstwcnshjqxg
umdryebvlapkovfktwcnuhjqxg
umdrqrbvlppkozfstwcnihjqxg
umdrylgvlapkozfstwrnihjqxg
umdryebvlapkozfstxcnihbqig
uvdryeevlappozfstwcnihjqxg
zmdryebvlapkozfstwcnihqqxt
umdryebvlapvozfstwenihiqxg
umdryebvlbpkozfsgwcnihjlxg
umdryhbvlapkozfstwcnihtqxw
umdreecvlapkozwstwcnihjqxg
umwryebvlapkoztsmwcnihjqxg
ukdryebvfapkozrstwcnihjqxg
umdrylbdlamkozfstwcnihjqxg
umdryebvlapoozwsmwcnihjqxg
umdryebvlapkozfqtwcnnzjqxg
umdryekvlapktzfstwcnohjqxg
umdryebvlapkozfstwcnihjwqo
umdrrebflapkogfstwcnihjqxg
umdryevvlapkozfztwctihjqxg
umdrybbvlapkozfstwcnihxaxg
umdryebvlapkozfsowcnphjqag
smdryebvlapbozfitwcnihjqxg
umdryebvtapiozfstwcnihjqxe
umdryebjlakkozfstwccihjqxg
umdryebvlapdozfshwckihjqxg
umnryebvlapiozfstwcnihlqxg
umdrycbvlapkjzfsnwcnihjqxg
umdryebvyaprozjstwcnihjqxg
ucdryebvlapkozfstwomihjqxg
umdryebvlagklzfstwcnihjqyg
umdryebvladkozfstwcnihjqjh
umdrwebvlapkozfstwdnicjqxg
umdryebvlapkmzfstwcniheqxr
umdryebvlapkjzfstwcviheqxg
umdrvebvlapkozfstwcbihjqmg
umdrfebvlapkoffstwcnihsqxg
umdryebvtarkazfstwcnihjqxg
umdryebvlapkozfstwcfihjcng
umdryebvlapkktostwcnihjqxg
uedryeevlapkozfstwcniijqxg
bmdryebylapkozfstwcnihjqog
umdryebvlmpkoztstwcnihjqeg
umdryepvlarkohfstwcnihjqxg
uwdryebvlapklzfstzcnihjqxg
umdryebklapkozfsswcbihjqxg
umdtyeavlapkozfstwsnihjqxg
umdryebvaapkozfhtfcnihjqxg
umdrpebvlapuozfstwvnihjqxg
umdryebvlapkozffmwcniijqxg
uqdpyebvlapkozfstwfnihjqxg
umdryebvlapuozdstwcnihjhxg
tmdryhbvlapkozfptwcnihjqxg
umdryevvmapkozfstwcnihjgxg
umdryeuvlapmozfstwcnihjwxg
umdryebqlzpkozfbtwcnihjqxg
umdryebvsapkozystwcniqjqxg
imdryebvlapkozfscwinihjqxg
umdryebvlzpkozustwcnmhjqxg
umdrypbvlapbozfsnwcnihjqxg
bmdryebvlapqoznstwcnihjqxg
umdrfebvlapaozfstwcnihxqxg
umdiyebvxapkozfstwcnchjqxg
umdrygbvlapkozfstwcnizjqxz
amdryedvlapkozfstwcnihfqxg
umdryebvvapzozfstwcnihjgxg
undryebvlapkzzfstjcnihjqxg
umdryvbvlapgozfrtwcnihjqxg
umdrkebvlapkozfstwcnihihxg
umdryebvrppkozfsowcnihjqxg
umdryebvlapktzfsdwclihjqxg
otdrdebvlapkozfstwcnihjqxg
mmdryebvlazkozfxtwcnihjqxg
umdryebvlapkozfsbwtnihjqxa
imqryebvrapkozfstwcnihjqxg
umdryebvlrpkozfscwcnihjqlg
uedryebvlapkoznsvwcnihjqxg
umdryebvlqpkozfstscnihjqxj
umerycbvlapkozfstwcnihjqxh
umdkykbvlapjozfstwcnihjqxg
"

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let parsed = parse input
    stopwatch.Stop()

    printfn "parsing input took %i milliseconds" stopwatch.ElapsedMilliseconds

    stopwatch.Restart()
    let res =
        parsed
        |> List.map (fun word -> createMap Map.empty word)
        |> List.map countTwosAndThrees
        |> List.map (fun (a,b) -> (normalize a, normalize b))
        |> List.fold (fun (twos, threes) (a, b) -> (twos+a,threes+b)) (0,0)

    stopwatch.Stop()
    printfn "the result of day 2 part 1 is: %i" ((fst res) * (snd res))
    printfn "calculation took %i milliseconds" stopwatch.ElapsedMilliseconds

    stopwatch.Restart()
    let oneDiffs =
        parsed
        |> List.map (fun p -> filterForOneCharDiff p parsed)
        |> List.filter (fun p -> p.Length = 1)
        |> List.map (fun p -> p.[0])

    let res2 =
        getEqualChars (oneDiffs.[0]) (oneDiffs.[1])
        |> String.Concat
    stopwatch.Stop()

    printfn "result of day 2 part 2 is:  %s" res2
    printfn "calculation took %i milliseconds" stopwatch.ElapsedMilliseconds

    Console.ReadKey() |> ignore
    0 // return an integer exit code
