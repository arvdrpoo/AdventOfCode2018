namespace Util

module Say =
    let hello name =
        printfn "Hello %s" name

module Arr =
    let tee a =
        printfn "%A" a
        a
