// continuation example

let divide ifZero ifSuccess top bottom =
    if (bottom = 0)
    then ifZero()
    else ifSuccess(top / bottom)

// three scenarios

// Scenario 1: pipe the result into a message
// ----------------------------------------
// setup the functions to print a message
let ifZero1 () = printfn "bad"
let ifSuccess1 x = printfn "good %i" x

// use partial application
let divide1  = divide ifZero1 ifSuccess1

//test
let good1 = divide1 6 3
let bad1 = divide1 6 0

// Scenario 2: convert the result to an option
// ----------------------------------------
// setup the functions to return an Option
let ifZero2() = None
let ifSuccess2 x = Some x
let divide2  = divide ifZero2 ifSuccess2

//test
let good2 = divide2 6 3
let bad2 = divide2 6 0

// Scenario 3: throw an exception in the bad case
// ----------------------------------------
// setup the functions to throw exception
let ifZero3() = failwith "div by 0"
let ifSuccess3 x = x
let divide3  = divide ifZero3 ifSuccess3

//test
let good3 = divide3 6 3
let bad3 = divide3 6 0

//wraping the continuation in a function

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

let divideByWorkflow x y w z = 
    let a = x |> divideBy y 
    match a with
    | None -> None  // give up
    | Some a' ->    // keep going
        let b = a' |> divideBy w
        match b with
        | None -> None  // give up
        | Some b' ->    // keep going
            let c = b' |> divideBy z
            match c with
            | None -> None  // give up
            | Some c' ->    // keep going
                //return 
                Some c'

let pipeInto (someExpression,lambda) =
    match someExpression with
    | None -> 
        None
    | Some x ->       
        x |> lambda

let divideByWorkflow x y w z = 
    let a = x |> divideBy y 
    pipeInto (a, fun a' ->
        let b = a' |> divideBy w
        pipeInto (b, fun b' ->
            let c = b' |> divideBy z
            pipeInto (c, fun c' ->
                Some c' //return 

                ))) 

let divideByResult x y w z = 
    pipeInto (x |> divideBy y, fun a ->
    pipeInto (a |> divideBy w, fun b ->
    pipeInto (b |> divideBy z, fun c ->
    Some c //return 
    )))

// answer
let strToInt str = 
    try 
        let i = int str 
        Some i
    with
    | _ -> None

strToInt("2");;
strToInt("x");;

type MaybeBuilder() =
    member this.Bind(m, f) =
        match m with
        | None -> None
        | Some a -> f a
    member this.Return(x) =
        Some x

let yourWorkflow = new MaybeBuilder();

let stringAddWorkflow x y z = 
    yourWorkflow 
        {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }    

// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

let strAdd str i = 
    yourWorkflow
        {
        let! a = strToInt str
        return a + i
        }

let (>>=) m f = Option.bind f m

let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"