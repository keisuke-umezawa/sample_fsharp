﻿module expressions


type Condition =
    | ConstCondition of bool
    | Not of Condition
    | And of Condition * Condition
    | Or of Condition * Condition
    | EqualTo of Expression * Expression
    | MoreThan of Expression * Expression
and Expression = 
    | Const of float
    | Var of string
    | Neg of Expression
    | Pow of Expression * Expression
    | Mul of Expression * Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Div of Expression * Expression
    | Exp of Expression
    | Log of Expression
    | Sin of Expression
    | Cos of Expression
    | When of Condition * Expression
    | Until of Condition * Expression
    static member ( ~+ ) x = x
    static member ( ~- ) x = Neg(x)
    static member ( + ) (x, y) = Add(x, y)
    static member ( - ) (x, y) = Sub(x, y)
    static member ( * ) (x, y) = Mul(x, y)
    static member ( / ) (x, y) = Div(x, y)
    static member ( + ) (x, n) = Add(x, Const(n))
    static member ( - ) (x, n) = Sub(x, Const(n))
    static member ( * ) (x, n) = Mul(x, Const(n))
    static member ( / ) (x, n) = Div(x, Const(n))
    static member ( + ) (n, y) = Add(Const(n), y)
    static member ( - ) (n, y) = Sub(Const(n), y)
    static member ( * ) (n, y) = Mul(Const(n), y)
    static member ( / ) (n, y) = Div(Const(n), y)


let (|UnaryOp|_|) (x : Expression) =
    match x with
    | Neg(e) -> Some(Neg, e)
    | Exp(e) -> Some(Exp, e)
    | Log(e) -> Some(Log, e)
    | Sin(e) -> Some(Sin, e)
    | Cos(e) -> Some(Cos, e)
    | _ -> None

let (|BinaryOp|_|) (x : Expression) =
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Sub(e1, e2) -> Some(Sub, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | Div(e1, e2) -> Some(Div, e1, e2)
    | Pow(e1, e2) -> Some(Pow, e1, e2)
    | _ -> None

let (|BinaryCondition|_|) (c : Condition) =
    match c with
    | And(c1, c2) -> Some(And, c1, c2)
    | Or(c1, c2) -> Some(Or, c1, c2)
    | _ -> None

let (|ComparisonCondition|_|) (c : Condition) =
    match c with
    | EqualTo(e1, e2) -> Some(EqualTo, e1, e2)
    | MoreThan(e1, e2) -> Some(MoreThan, e1, e2)
    | _ -> None

let (|CommutativeOp|_|) (x : Expression) =
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | _ -> None

let (|Func|_|) (x : Expression) =
    match x with
    | Exp(e) -> Some(Exp, e)
    | Log(e) -> Some(Log, e)
    | Sin(e) -> Some(Sin, e)
    | Cos(e) -> Some(Cos, e)
    | _ -> None

let (|Linear|_|) (x : Expression) = 
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Sub(e1, e2) -> Some(Sub, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | _ -> None

let rec (|Constant|_|) (x : Expression) =
    match x with
    | UnaryOp(op, Constant(c)) -> Some(x)
    | BinaryOp(op, Constant(c1), Constant(c2)) -> Some(x)
    | Const(n) -> Some(x)
    | Neg(Constant(c)) -> Some(x)
    | _ -> None

let isPrimitive (x : Expression) : bool = 
    match x with
    | Var(v) -> true
    | Const(n) -> true
    | _ -> false

let isEqualBinaryOp op1 op2 : bool = 
    let c = Const(1.)
    op1(c, c) = op2(c, c)

let rec Dismantle x = 
    match x with
    | Constant(c) -> c, Const(1.)
    | Mul(Const(n), e) -> 
        let (n1, e1) = Dismantle e
        n * n1, e1
    | Mul(e, Const(n)) -> 
        let (n1, e1) = Dismantle e
        n * n1, e1
    | Mul(e1, e2) -> 
        let (n1, y1) = Dismantle e1
        let (n2, y2) = Dismantle e2
        n1 * n2, y1 * y2
    | Div(e, Const(n)) ->
        let (m, y) = Dismantle e
        m / n, y
    | _ -> Const(1.), x

let LargerThan x y = 
    let (n1, e1) = Dismantle x in
    let (n2, e2) = Dismantle y in
    if e1 > e2 then
        true
    elif e1 = e2 then
        x > y
    else
        false

let Factors x = 
    let rec FactorsImpl x lst = 
        match x with
        | Mul(e1, e2) -> FactorsImpl e2 ( FactorsImpl e1 lst)
        | _ -> x::lst
    FactorsImpl x []

let HasSameFactor x y =
    not (Set.intersect (set (Factors x)) (set (Factors y))).IsEmpty

let RemoveSameFactors lst1 lst2 =
    let intersect = Set.intersect (set lst1) (set lst2) in
    Set.toList ((set lst1) - intersect), Set.toList ((set lst2) - intersect)

let rec Muln lst = 
    List.fold ( * ) (Const 1.) lst

let rec Inverse x = 
    match x with
    | Const(n) -> Const( 1. / n)
    | Var(s) -> Pow(x, Const(-1.))
    | Neg(e) -> Neg(Inverse(e))
    | Mul(e1, e2) -> Mul(Inverse(e1), Inverse(e2))
    | Pow(e1, e2) -> Pow(e1, -e2)
    | Div(e1, e2) -> Div(e2, e1)
    | _ -> Pow(x, Const(-1.))

let rec MulnInversed lst = 
    Muln (List.map Inverse lst)

let Elements x =
    let rec ElementImpl x lst =
        match x with
        | Const(n) -> lst
        | BinaryOp(op, e1, e2) -> ElementImpl e1 (ElementImpl e2 lst)
        | Func(f, e) -> ElementImpl e lst
        | _ -> x::lst
    Set.toList (set (ElementImpl x []))

let Terms x =
    let rec TermsImpl x lst = 
        match x with
        | Add(e1, e2) -> TermsImpl e1 (TermsImpl e2 lst)
        | _ -> x::lst
    TermsImpl x []

let rec Addn lst =
    List.fold (+) (Const 0.) lst

let rec Depends e x = 
    List.exists (fun n -> n = x) (Elements e)

let getGcd (n : int) (m : int) : int =
    let rec getGcdImpl n m =
        if n = 0 then
            m
        else
            getGcdImpl (m % n) n
    if n > m then
        getGcdImpl m n
    else
        getGcdImpl n m

let addChangeFlag f x =
    let y = f x
    (y <> x, y)

let tryToCall f op e1 e2 =
    let (isModified2, a1) = addChangeFlag f e1
    let (isModified1, a2) = addChangeFlag f e2
    if isModified1 || isModified2 then
        op(a1, a2) |> f
    else
        op(e1, e2)

//-------------------------------------------------------------------------------------------------
// Format
//-------------------------------------------------------------------------------------------------
let OpName (e: Expression) : string =
    match e with
    | Add(e1, e2) -> "+"
    | Sub(e1, e2) -> "-"
    | Mul(e1, e2) -> "*"
    | Div(e1, e2) -> "/"
    | Pow(e1, e2) -> "^"
    | _ -> failwith(sprintf "Unrecoginized function [%A]" e)

let FunName (e: Expression) (a: string) : string =
    match e with
    | Exp(x) -> sprintf "e^(%s)" a
    | Log(x) -> sprintf "log(%s)" a
    | Sin(x) -> sprintf "sin(%s)" a
    | Cos(x) -> sprintf "cos(%s)" a
    | _ -> failwith(sprintf "Unrecoginized function [%A]" e)


let Format e : string = 
    let requireParenthesis x : bool =
        match x with
        | Mul(e1, e2) -> true
        | Div(e1, e2) -> true
        | _ -> false
    let rec FormatImpl higher needParenthesis x : string =
        match x with
        | Var(s) -> s
        | Const(n) -> sprintf "%f" n
        | Neg(x) -> sprintf "-%s" (FormatImpl "neg" true x)
        | BinaryOp(op, e1, e2) -> 
            let t = (requireParenthesis x)
            let s = sprintf "%s %s %s" (FormatImpl (OpName x) t e1) (OpName x) (FormatImpl (OpName x) t e2)
            if needParenthesis && higher <> "" && (OpName x) <> higher then
                "(" + s + ")"
            else
                s
        | _ -> failwith(sprintf "unrecognized expression [%A]" x)
    FormatImpl "" false e
    
    
//-------------------------------------------------------------------------------------------------
// Sort
//-------------------------------------------------------------------------------------------------
let rec SortImpl x = 
    match x with 
    | CommutativeOp(op, e1, e2) when LargerThan e1 e2 ->
         op(e2, e1) |> SortImpl
    // add
    | CommutativeOp(op1, e1, CommutativeOp(op2, e2, e3)) 
        when LargerThan e1 e2 && isEqualBinaryOp op1 op2 ->
        op1(e2, op1(e1, e3)) |> SortImpl
    | CommutativeOp(op1, CommutativeOp(op2, e1, e2), e3) 
        when isEqualBinaryOp op1 op2 -> 
        op1(e1, op1(e2, e3)) |> SortImpl
    | CommutativeOp(op, CommutativeOp(op1, e1, e2), CommutativeOp(op2, e3, e4)) 
        when LargerThan e2 e3 && isEqualBinaryOp op op1 && isEqualBinaryOp op op2 -> 
        op(op(e1, e3), op(e2, e4)) |> SortImpl
    // subtract
    | Sub(Add(e1, e2), e3) when LargerThan e2 e3 -> Add(Sub(e1, e3), e2) |> SortImpl
    // binary operator
    | BinaryOp(op, e1, e2) -> tryToCall SortImpl op e1 e2
    // other
    | _ -> x

let rec SortCondition c =
    match c with
    | And(c1, c2) when c1 > c2 -> And(c2, c1) |> SortCondition
    | And(c, And(c1, c2)) when c > c1 -> And(c1, And(c, c2)) |> SortCondition
    | And(And(c1, c2), c) when c2 > c -> And(And(c1, c), c2) |> SortCondition
    | And(And(c1, c2), And(c3, c4)) when c2 > c3 -> And(And(c1, c3), And(c2, c4)) |> SortCondition
    | Or(c1, c2) when c1 > c2 -> Or(c2, c1) |> SortCondition
    | Or(c, Or(c1, c2)) when c > c1 -> Or(c1, Or(c, c2)) |> SortCondition
    | Or(Or(c1, c2), c) when c2 > c -> Or(Or(c1, c), c2) |> SortCondition
    | Or(Or(c1, c2), Or(c3, c4)) when c2 > c3 -> Or(Or(c1, c3), Or(c2, c4)) |> SortCondition
    | BinaryCondition(op, c1, c2) -> tryToCall SortCondition op c1 c2
    | EqualTo(c1, c2) when c1 > c2 -> EqualTo(c2, c1) |> SortCondition
    | _ -> c

let Sort x = SortImpl x 


//-------------------------------------------------------------------------------------------------
// Expand
//-------------------------------------------------------------------------------------------------
let rec ExpandImpl x = 
    match x with
    | Mul(e1, Add(e2, e3)) -> Add(e1 * e2, e1 * e3) |> Sort |> ExpandImpl
    | Mul(e1, Sub(e2, e3)) -> Sub(e1 * e2, e1 * e3) |> Sort |> ExpandImpl
    | Sub(e1, Add(e2, e3)) -> e1 - e2 - e3  |> Sort |> ExpandImpl
    | BinaryOp(op, e1, e2) -> tryToCall ExpandImpl op e1 e2
    | _ -> x

let Expand x = Sort x |> ExpandImpl

let rec ExpandCondition c =
    match c with
    | Not(And(c1, c2)) -> Or(Not(c1), Not(c2)) |> SortCondition |> ExpandCondition
    | Not(Or(c1, c2)) -> And(Not(c1), Not(c2)) |> SortCondition |> ExpandCondition
    | And(c1, c2) -> tryToCall ExpandCondition And c1 c2
    | Or(c1, c2) -> tryToCall ExpandCondition Or c1 c2
    | Not(c) when c <> ExpandCondition c-> Not(ExpandCondition c) |> SortCondition |> ExpandCondition
    | _ -> c
//-------------------------------------------------------------------------------------------------
// SimplifyConstant
//-------------------------------------------------------------------------------------------------
let SimplifyConstant x = 
    let rec ToFraction x = 
        match x with
        | Div(Const(c1), Const(c2)) -> x 
        | BinaryOp(op, c1, c2) ->
            op((ToFraction c1), (ToFraction c2))
        | Const(c) -> x / 1.
        | _ -> x
    let rec SimplifyConstantImpl x = 
        match x with
        | Div(Const(n1), Const(n2)) -> x
        | Add(Div(Const(a1), Const(b1)), Div(Const(a2), Const(b2))) ->
            Const(a1 * b2 + a2 * b1) / Const(b1 * b2)
        | Sub(Div(Const(a1), Const(b1)), Div(Const(a2), Const(b2))) ->
            Const(a1 * b2 - a2 * b1) / Const(b1 * b2)
        | Mul(Div(Const(a1), Const(b1)), Div(Const(a2), Const(b2))) ->
            Const(a1 * a2) / Const(b1 * b2)
        | Div(Div(Const(a1), Const(b1)), Div(Const(a2), Const(b2))) ->
            Const(a1 * b2) / Const(b1 * a2)
        | Neg(c) -> -1.0 * (SimplifyConstantImpl c)
        | BinaryOp(op, c1, c2) -> tryToCall SimplifyConstantImpl op c1 c2
        | _ -> x
    let rec Reduce x =
        match x with
        | Div(c, Const(1.)) -> c |> Reduce
        | Div(Const(c1), Const(c2)) when c1 % 1.0 = 0. && c2 % 1.0 = 0.
            -> 
            let gcd = (getGcd (int c1) (int c2)) in
            let n1 = c1 / (float gcd) in
            let n2 = c2 / (float gcd) in
            if n2 = 1. then
                Const(n1)
            else
                Const(n1) / Const(n2)
        | _ -> x
    x |> ToFraction |> SimplifyConstantImpl |> Reduce

//-------------------------------------------------------------------------------------------------
// SimplifyCondition
//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------
// Simplify
//-------------------------------------------------------------------------------------------------
let rec Simplify x = 
    let rec SimplifyImpl firstTry x = 
        let ChangeTermOrder op e1 e2 e3 =
            let e12 = op(e1, e2) |> Expand |> SimplifyImpl true in
            if e12 <> op(e1, e2) then
                op(e12, e3) |> Expand |> SimplifyImpl true
            else
                let e13 = op(e1, e3) |> Expand |> SimplifyImpl true in
                if e13 <> op(e1, e3) then
                    op(e13, e2) |> Expand |> SimplifyImpl true
                else
                    x
        match x with
        // constant
        | Constant(c) -> c |> SimplifyConstant
        // neg
        | Neg(Neg(e)) -> e |> Expand |> SimplifyImpl true
        // add
        | Add(Const(0.), e) -> e |> Expand |> SimplifyImpl true
        | Add(e1, Neg(e2)) -> Sub(e1, e2) |> Expand |> SimplifyImpl true
        | Add(e1, e2) when
            let (n1, x1) = Dismantle e1
            let (n2, x2) = Dismantle e2
            x1 = x2
            -> 
            let (n1, x1) = Dismantle e1
            let (n2, x2) = Dismantle e2
            (SimplifyConstant (n1 + n2)) * x1 |> Expand |> SimplifyImpl true
        // power
        | Pow(e, Const(1.)) -> e |> SimplifyImpl true
        | Pow(e, Const(0.)) -> Const(1.) 
        | Pow(Const(1.), e) -> Const(1.) 
        | Pow(Pow(x, n1), n2) -> Pow(x, n1 * n2) |> Expand |> SimplifyImpl true
        // multiply
        | Mul(Const(0.), e) -> Const(0.)
        | Mul(Const(1.), e) -> e |> Expand |> SimplifyImpl true
        | Mul(e1, e2) when e1 = e2 -> Pow(e1, Const(2.)) |> Expand |> SimplifyImpl true
        | Mul(e1, Pow(e2, n)) when e1 = e2 -> Pow(e1, (n + Const(1.))) |> Expand|> SimplifyImpl true
        | Mul(Pow(e1, n1), Pow(e2, n2)) when e1 = e2 -> Pow(e1, n1 + n2) |> Expand|> SimplifyImpl true
        // subtract
        | Sub(e1, e2) when e1 = e2 -> Const(0.)
        | Sub(e1, Neg(e2)) -> Add(e1, e2) |> Expand |> SimplifyImpl true
        // divide
        | Div(e1, e2) ->
            let (n1, x1) = Dismantle e1
            let (n2, x2) = Dismantle e2
            (SimplifyConstant n1 / n2) * x1 * (MulnInversed (Factors x2)) |> Expand |> SimplifyImpl true
        // divide
        | When(ConstCondition true, e) -> e |> SimplifyImpl true
        | When(ConstCondition false, e) -> Const 0.
        | When(c, e) when firstTry-> When(SimplifyCondition c, Simplify e) |> SimplifyImpl false
        // binary operator
        | BinaryOp(op, e1, e2) when firstTry -> 
            let e1 = e1 |> Expand |> SimplifyImpl true
            let e2 = e2 |> Expand |> SimplifyImpl true
            op(e1, e2) |> Expand |> SimplifyImpl false
        // change order
        | CommutativeOp(op1, e1, CommutativeOp(op2, e2, e3))
            when isEqualBinaryOp op1 op2 -> ChangeTermOrder op1 e1 e2 e3
        // other
        | _ -> x
    Expand x |> SimplifyImpl true
and SimplifyCondition (c : Condition)= 
    match c with
    | Not(ConstCondition b) -> ConstCondition (not b)
    | Not(Not(c)) -> c |> SimplifyCondition
    | And(ConstCondition b1, ConstCondition b2) -> ConstCondition(b1 && b2)
    | Or(ConstCondition b1, ConstCondition b2) -> ConstCondition(b1 || b2)
    | EqualTo(e1, e2) when e1 = e2 -> ConstCondition(true)
    | EqualTo(Const c1, Const c2) when c1 <> c2 -> ConstCondition(false)
    | MoreThan(Const c1, Const c2) when c1 > c2 -> ConstCondition(true)
    | MoreThan(Const c1, Const c2) when c1 <= c2 -> ConstCondition(false)
    | ComparisonCondition(op, e1, e2) ->
        let ee1 = Simplify e1
        let ee2 = Simplify e2
        if ee1 <> e1 || ee2 <> e2 then
            op(ee1, ee2) |> SimplifyCondition
        else
            op(ee1, ee2)
    | _ -> c

//-------------------------------------------------------------------------------------------------
// differentiate
//-------------------------------------------------------------------------------------------------
let rec OrderImpl e x = 
    if e = x then
        1.
    else
        match e with
        | Mul(e1, e2) -> (OrderImpl e1 x) + (OrderImpl e2 x)
        | Pow(e1, Const(n)) -> (OrderImpl e1 x) * n
        | Sub(e1, e2) -> (OrderImpl e1 x) - (OrderImpl e2 x)
        | Var(s) -> 0.
        | Neg(e1) -> OrderImpl e1 x
        | Constant(c) -> 0.
        | _ -> failwith(sprintf "error in OrderImpl [%A]" e) 
    
let Order e x =
    let e = Simplify e in
    OrderImpl e x


let Differentiate(e, x, times) =
    let rec DifferentiateImpl term x times =
        if times <= 0 then
            term
        else
            if Depends term x then
                let order = Order term x in
                (DifferentiateImpl (order * term / x) x (times - 1))
            else
                Const(0.)
    if times <= 0 then
        e
    else
        let e = Simplify e in
        (Simplify (Addn (List.map (fun t -> DifferentiateImpl t x times) (Terms e))))

//-------------------------------------------------------------------------------------------------
// integrate
//-------------------------------------------------------------------------------------------------
let Integrate(e, x) =
    let IngegrateImpl term x =
        let order = Order term x in
        term * x / (order + 1.)
    (Simplify (Addn (List.map (fun t -> IngegrateImpl t x) (Terms (Simplify e)))))
        