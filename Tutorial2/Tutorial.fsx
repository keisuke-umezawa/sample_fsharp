open System

type Term<'T when 'T:equality> =
    | Value of 'T
    | Minus of Term<'T>
    | Add of Term<'T> * Term<'T>
    | Sum of list<Term<'T>>
    | Other


let isAdd term = 
    match term with 
    | Add (t1, t2) -> true
    | _ -> false

let hasAdd lst =
    List.exists isAdd lst

let isMinus term = 
    match term with 
    | Minus (t) -> true
    | _ -> false

let isSum term = 
    match term with 
    | Sum (lst) -> true
    | _ -> false


let hasSum lst =
    List.exists isSum lst

let tryGetValue term =
    match term with
    | Value (t) -> Some (t)
    | _ -> None
let tryGetSumList term =
    match term with
    | Sum (lst) -> Some (lst)
    | _ -> None
let tryGetOtherList term =
    match term with
    | Value (t) -> None
    | Sum (lst) -> None
    | _ -> Some(term)

let rec optimizeTerm term =
    match term with
    | Value (t) -> Value (t)
    | Minus (term1) ->
        match term1 with
        | Minus (term2) -> optimizeTerm term2
        | Value (t) -> Value (-t)
        | Sum (list) -> Sum(list |> List.map Minus)
        | _ -> Minus (optimizeTerm term1)
    | Add (term1, term2) -> optimizeTerm (Sum ([term1; term2]))
    | Sum (lst) -> Sum (optimizeSum lst)
    | Other -> Other
and optimizeSum (lst:list<Term<'T>>) = 
    match lst with
    | [] -> []
    | _ ->
        let optimizedList = List.map optimizeTerm lst
        let sumList = optimizedList |> List.choose tryGetSumList |> List.concat
        let valueList = [Value(optimizedList |> List.choose tryGetValue |> List.average)]
        let otherList = optimizedList |> List.choose tryGetOtherList 
        List.concat [sumList; valueList; otherList]
        
    
let v1 = Value (1.)
let v2 = Value (2.)
let v3 = Value (3.)

let term1 = Add (v1, v2)
let term2 = Add (v1, Minus (v2))
let term3 = Add (v1, Add (Other, Minus (v2)))
let term4 = Add (v1, Minus (Add (v2, v3)))
let term5 = Minus (Add (v1, Minus (Add (v2, v3))))

let opt1 = optimizeTerm term1
let opt2 = optimizeTerm term2
let opt3 = optimizeTerm term3
let opt4 = optimizeTerm term4
let opt5 = optimizeTerm term5