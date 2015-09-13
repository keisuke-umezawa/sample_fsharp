type Expression =
    | Const of float
    | X
    | Neg of Expression
    | Add of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Max of Expression * Expression
    | Exp of Expression
    | Log of Expression
    | Sin of Expression
    | Cos of Expression


let (|BinaryOp|_|) (x : Expression) =
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | Div(e1, e2) -> Some(Div, e1, e2)
    | Max(e1, e2) -> Some(Max, e1, e2)
    | _ -> None
    
let (|CommutativeOp|_|) (x : Expression) =
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | Max(e1, e2) -> Some(Max, e1, e2)
    | _ -> None

let (|UnaryOp|_|) (x : Expression) =
    match x with
    | Exp(e) -> Some(Exp, e)
    | Log(e) -> Some(Log, e)
    | Sin(e) -> Some(Sin, e)
    | Cos(e) -> Some(Cos, e)
    | _ -> None

let rec Simplify x : Expression =
    match x with
    | Add(Const(n1), Const(n2)) -> Const(n1 + n2)
    | Mul(Const(n1), Const(n2)) -> Const(n1 * n2)
    | Div(Const(n1), Const(n2)) -> Const(n1 / n2)
    | Neg(Const(n)) -> Const(-n)
    | Neg(Neg(e)) -> e |> Simplify
    | Neg(Add(e1, e2)) -> Add(Simplify (Neg e1), Simplify (Neg e2)) |> Simplify
    | Neg(e) -> Neg(Simplify e)
    | Add(Const(0.), e) -> e |> Simplify
    | Add(Neg(e1), e2) when e1 = e2 -> Const(0.)
    | Add(e1, e2) when e1 = e2 -> Mul(Const(2.0), e1) |> Simplify
    | Add(e1, Mul(Const(n2), e3)) when e1 = e3 -> Mul(Const(1.0 + n2), e1) |> Simplify
    | Mul(Add(e1, e2), e3) -> Add(Mul(e1, e3), Mul(e2, e3)) |> Simplify
    | Mul(e3, Add(e1, e2)) -> Add(Mul(e1, e3), Mul(e2, e3)) |> Simplify
    | Mul(Const(1.), e) -> e |> Simplify
    | Mul(Const(0.), e) -> Const(0.)
    | Mul(Div(Const(n), e1), e2) -> Mul(Const(n), Div(e2, e1)) |> Simplify
    | Mul(e1, Div(Const(n), e2)) -> Mul(Const(n), Div(e1, e2)) |> Simplify
    | Mul(Neg(e1), e2) -> Neg(Mul(e1, e2)) |> Simplify
    | Mul(e1, Neg(e2)) -> Neg(Mul(e1, e2)) |> Simplify
    | Div(Const(0.), e) -> Const(0.)
    | Div(e, Const(1.)) -> e |> Simplify
    | Div(e, Const(n)) -> Mul(e, Const(1. / n)) |> Simplify
    | Div(Neg(e1), e2) -> Neg(Div(e1, e2)) |> Simplify
    | Div(e1, Neg(e2)) -> Neg(Div(e1, e2)) |> Simplify
    | Div(e1, e2) when e1 = e2 -> Const(1.)
    | Max(Const(n1), Const(n2)) when n1 > n2 -> Const(n1)
    | Max(Const(n1), Const(n2)) when n1 <= n2 -> Const(n2)
    | CommutativeOp(op, Const(n1), CommutativeOp(op2, Const(n3), e4)) 
        when op(e4, e4) = op2(e4, e4) ->
            op(Simplify(op(Const(n1), Const(n3))), e4) |> Simplify
    | CommutativeOp(op, CommutativeOp(op1, Const(n1), e2), CommutativeOp(op2, Const(n3), e4)) 
        when op(e2, e4) = op1(e2, e4) && op(e2, e4) = op2(e2, e4) ->
            op(Simplify(op(Const(n1), Const(n3))), op(e2, e4)) |> Simplify
    | CommutativeOp (op, e1, CommutativeOp(op1, e2, e3)) 
        when e1 > e2 && op(e1, e2) = op1(e1, e2) ->
            op(e2, op(e1, e3)) |> Simplify
    | CommutativeOp (op, CommutativeOp(op1, e1, e2), e3) 
        when e1 < op1(e2, e3) && op(e1, e2) = op1(e1, e2) ->
            op(e1, op(e2, e3)) |> Simplify
    | CommutativeOp (op, e1, e2) when e1 > e2 -> op(e2, e1) |> Simplify
    | UnaryOp (op, e) ->
        let es = Simplify e
        if es <> e then
            op(Simplify e) |> Simplify
        else
            op(e)
    | BinaryOp (op, e1, e2) ->
        let e1s = Simplify e1
        let e2s = Simplify e2
        if e1s <> e1 || e2s <> e2 then
            op(Simplify e1, Simplify e2) |> Simplify
        else
            op(e1, e2)
    | _ -> x

//    | Add(Mul(Const(n1), e2), Mul(Const(n3), e4)) when e2 = e4 -> Mul(Const(n1 + n3), e2) |> Simplify
//    | Add(Const(n1), Add(Const(n2), e3)) -> Add(Const(n1 + n2), e3) |> Simplify
//    | Add(Add(Const(n1), e2), Add(Const(n3), e4)) -> Add(Const(n1 + n3), Add(e2, e4)) |> Simplify
//    | Add(e1, Add(e2, e3)) when e1 > e2 -> Add(e2, Add(e1, e3)) |> Simplify
//    | Add(Add(e1, e2), e3) when e1 < Add(e1, e2) -> Add(e1, Add(e2, e3)) |> Simplify

let c1 = Const 1.
let c2 = Const 2.
let c3 = Const 3.

let e1 = Add(c1, Add(c3, X))
Simplify e1 // Add(Const(4.) X)
let e2 = Neg(Add(c1, Add(c1, Add(c1, Add(c1, X)))))
Simplify e2 // Add (Const -4.0,Neg X)
let e3 = Add(Add(X, Add(c1, Add(X, Add(Add(c2, Add(c1, X)), Add(c2, Add(c1, X)))))),
             Add(c1, Add(c1, Add(c2, X))))
Simplify e3 // Add (Const 11.0,Add (Add (Add (Add (X,X),X),X),X))
let e4 = Mul(c2, Add(c3, X))
Simplify e4 // Add(Const 6.0, Mul (Const 2.0, X))
let e11 = Add(Add(c3, X), c1)
let e5 = Add(e1, Neg(e11))
Simplify e5 // Add (Const 4.0,Add (Neg (Add (Const 4.0,X)),X))
let e6 = Mul(Add(X, Mul(c1, Add(X, Add(Add(c2, Mul(c1, X)), Mul(c2, Add(c1, X)))))),
             Add(c1, Mul(c1, Add(c2, X))))
Simplify e6
//  Add
//    (Const 12.0,
//     Add
//       (Add
//          (Add
//             (Add (Add (Mul (Const 2.0,X),X),X),
//              Add
//                (Add (Mul (Const 4.0,X),Mul (X,X)),
//                 Add (Mul (Mul (Const 2.0,X),X),Mul (X,X)))),
//           Add
//             (Add (Mul (Const 2.0,Mul (Const 2.0,X)),Mul (Const 2.0,X)),
//              Mul (Const 2.0,X))),Add (Add (Mul (X,X),X),Mul (Const 2.0,X))))
let e7 = Max(Const(2.), Const(3.))
Simplify e7 // Const 3.
let e8 = Max(Add(Const(2.), Neg(Const(-2.))), Const(3.))
Simplify e8 // Const 4.
let e9 = Mul(X, Const 0.)
Simplify e9 // Const 0.

let ee1 = Add(c1, X)
let ee2 = Mul(ee1, Mul (ee1, ee1))
Simplify ee2
//  Add
//    (Const 1.0,
//     Add
//       (Add (Add (Mul (Mul (X,X),X),Mul (X,X)),Add (Mul (X,X),X)),
//        Add (Add (X,X),Mul (X,X))))
let ee3 = Mul(c1, Mul(X, Mul(c2, X)))
let ee4 = Mul(ee3, ee3)
Simplify ee4 // Mul (Const 4.0,Mul (Mul (X,X),Mul (X,X)))

let ee5 = Div(ee3, ee3)
Simplify ee5 // Const 1.0