// compuation expresison in practice

let log p  = printfn "expression is %A" p

let loggedWorkfow =
    let x = 42
    log x
    let y = 43
    log y
    let z = x + y
    log z
    // return
    z
// results
//expression is 42
//expression is 43
//expression is 85

type LoggingBuilder() =
    let log p  = printfn "expression is %A" p

    member this.Bind(x, f) =
        log x
        f x

    member this.Return(x) =
        x

let logger = new LoggingBuilder()

let loggedWorkflow = 
    logger
        {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
        }



// safe division

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top / bottom)

let divideByWorkflow init x y z =
    let a = init |> divideBy x
    match a with
    | None -> None
    | Some a' ->
        let b = a' |> divideBy y
        match b with
        | None -> None 
        | Some b' ->
            let c = b' |> divideBy z
            match c with
            | None -> None
            | Some c' ->
                //return
                Some c'
let good = divideByWorkflow 12 3 2 1
let bad = divideByWorkflow 12 3 0 1

// maybe monad
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) =
        Some x

let maybe = new MaybeBuilder()

let divideByWorkflow init x y z =
    maybe
        {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
        }

let good = divideByWorkflow 12 3 2 1
let bad = divideByWorkflow 12 3 0 1

// chains of "or else" tests
let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("CA","California"); ("NY","New York") ] |> Map.ofList

let multiLookup key =
    match map1.TryFind key with
    | Some result1 -> Some result1   // success
    | None ->   // failure
        match map2.TryFind key with
        | Some result2 -> Some result2 // success
        | None ->   // failure
            match map3.TryFind key with
            | Some result3 -> Some result3  // success
            | None -> None // failure
multiLookup "A" |> printfn "Result for A is %A" 
multiLookup "CA" |> printfn "Result for CA is %A" 
multiLookup "X" |> printfn "Result for X is %A" 

// orElse monad
type OrElseBuilder() =
    member this.ReturnFrom (x) = x
    member this.Combine (a, b) = 
        match a with
        | Some _ -> a // a succeeds -- use if
        | None -> b   // a faiss -- use b instead
    member this.Delay (f) = f()
    
let orElse = new OrElseBuilder()

let multiLookup key = orElse {
    return! map1.TryFind key
    return! map2.TryFind key
    return! map3.TryFind key
    }
multiLookup "A" |> printfn "Result for A is %A" 
multiLookup "CA" |> printfn "Result for CA is %A" 
multiLookup "X" |> printfn "Result for X is %A" 

// Axynchronous calls with callback
open System.Net
let req1 = HttpWebRequest.Create("http://tryfsharp.org")
let req2 = HttpWebRequest.Create("http://google.com")
let req3 = HttpWebRequest.Create("http://bing.com")

req1.BeginGetResponse((fun r1 -> 
    use resp1 = req1.EndGetResponse(r1)
    printfn "Downloaded %O" resp1.ResponseUri

    req2.BeginGetResponse((fun r2 -> 
        use resp2 = req2.EndGetResponse(r2)
        printfn "Downloaded %O" resp2.ResponseUri

        req3.BeginGetResponse((fun r3 -> 
            use resp3 = req3.EndGetResponse(r3)
            printfn "Downloaded %O" resp3.ResponseUri

            ),null) |> ignore
        ),null) |> ignore
    ),null) |> ignore

async {
    use! resp1 = req1.AsyncGetResponse()
    printfn "Downloaded %O" resp1.ResponseUri
    use! resp2 = req2.AsyncGetResponse()
    printfn "Downloaded %O" resp2.ResponseUri
    use! resp3 = req3.AsyncGetResponse()
    printfn "Downloaded %O" resp3.ResponseUri
    } |> Async.RunSynchronously
// Summary
// common thing : "doing something behind the scenes"