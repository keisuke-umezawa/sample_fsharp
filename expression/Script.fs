//#load "Library1.fs"
module unittests

open expressions
open NUnit.Framework
open FsUnit

let x = Var "x"
let y = Var "y"
let z = Var "z"

let c1 = Const(1.)
let c2 = Const(2.)
let c3 = Const(3.)

let ctrue = ConstCondition(true)
let cfalse = ConstCondition(false)
let cequal = EqualTo(c1, c2)

[<TestFixture>] 
type ``expression tests`` ()=
   [<Test>] member test.
    ``test case 01`` ()=
           Format(Sort((x + y * x + c3) + (x + c1))) |> should equal "1.000000 + 3.000000 + x + x + x * y"
   [<Test>] member test.
    ``test case 02`` ()=
           Format(Sort((x + c1) + (x + c1))) |> should equal "1.000000 + 1.000000 + x + x"
   [<Test>] member test.
    ``test case 03`` ()=
           Format(Expand(y * (x + c1))) |> should equal "1.000000 * y + x * y"
   [<Test>] member test.
    ``test case 04`` ()=
           Format(Expand((x + c1) * y)) |> should equal "1.000000 * y + x * y"
   [<Test>] member test.
    ``test case 05`` ()=
           Format(Expand((x + c1) * (x - y))) |> should equal "1.000000 * x - 1.000000 * y + x * x - x * y"
   [<Test>] member test.
    ``test case 06`` ()=
           Format(Expand((x + c1) - (x + c1))) |> should equal "1.000000 - 1.000000 + x - x"
   [<Test>] member test.
    ``test case 07`` ()=
           Format(Simplify((x + c1) - (x + c1))) |> should equal "0.000000"
   [<Test>] member test.
    ``test case 08`` ()=
           Format(Simplify(x + x)) |> should equal "2.000000 * x"
   [<Test>] member test.
    ``test case 09`` ()=
           Format(Simplify(c2 * x + c2 * x)) |> should equal "4.000000 * x"
   [<Test>] member test.
    ``test case 10`` ()=
           Format(Simplify(x + c3 * x)) |> should equal "4.000000 * x"
   [<Test>] member test.
    ``test case 11`` ()=
           Format(Simplify(c2 * x +  c3 * x)) |> should equal "5.000000 * x"
   [<Test>] member test.
    ``test case 12`` ()=
           Format(Simplify(c2 * x * y +  c3 * x * y)) |> should equal "5.000000 * x * y"
   [<Test>] member test.
    ``test case 13`` ()=
           Format(Sort(z + c2 * x)) |> should equal "2.000000 * x + z"
   [<Test>] member test.
    ``test case 14`` ()=
           Format(Simplify(c2 * x + (x * y + c2 * x + z))) |> should equal "4.000000 * x + z + x * y"
   [<Test>] member test.
    ``test case 15`` ()=
           Format(Simplify((x + x)/ x)) |> should equal "2.000000"
   [<Test>] member test.
    ``test case 16`` ()=
           Format(Simplify((x * y)/ x)) |> should equal "y"
   [<Test>] member test.
    ``test case 17`` ()=
           Format(Simplify( (x + (-y)))) |> should equal "x - y"
   [<Test>] member test.
    ``test case 18`` ()=
           Format(Simplify((x + c1) * (y + c1) * z)) |> should equal "z + x * z + x * y * z + y * z"
   [<Test>] member test.
    ``test case 19`` ()=
           Format(Simplify(Pow(x, c1) * Pow(x, c2))) |> should equal "x ^ 3.000000"
   [<Test>] member test.
    ``test case 20`` ()=
           Format(Simplify(Pow(x, c1))) |> should equal "x"
   [<Test>] member test.
    ``test case 21`` ()=
           Elements(c2 * x + (x * y + c2 * x + z)) |> should equal [Var "x"; Var "y"; Var "z"]
   [<Test>] member test.
    ``test case 22`` ()=
           Format(Differentiate(3. * x * x + 2. * x, x, 1)) |> should equal "2.000000 + 6.000000 * x"
   [<Test>] member test.
    ``test case 23`` ()=
           Format(Differentiate(4. * x * x + 2. * x, x, 2)) |> should equal "8.000000"
   [<Test>] member test.
    ``test case 24`` ()=
           Format(Integrate(4. * x * x + 2. * y + 3., x)) |> should equal "3.000000 * x + (4.000000 / 3.000000) * (x ^ 3.000000) + 2.000000 * x * y"
   [<Test>] member test.
    ``test case 25`` ()=
           Format(Differentiate(1. / x, x, 1)) |> should equal "-1.000000 * (x ^ -2.000000)"
   [<Test>] member test.
    ``test case 26`` ()=
           And(cfalse, ctrue) |> SortCondition |> should equal (And(cfalse, ctrue))
   [<Test>] member test.
    ``test case 27`` ()=
           And(ctrue, cfalse) |> SortCondition |> should equal (And(cfalse, ctrue))
   [<Test>] member test.
    ``test case 28`` ()=
           ctrue |> should equal ctrue
   [<Test>] member test.
    ``test case 29`` ()=
           And(ctrue, And(cfalse, cfalse)) |> SortCondition |> should equal (And(cfalse, And(cfalse, ctrue)))
   [<Test>] member test.
    ``test case 30`` ()=
           And(And(ctrue, cfalse), And(ctrue, cfalse)) |> SortCondition |> 
           should equal (And(cfalse, And(cfalse, And(ctrue, ctrue))))
   [<Test>] member test.
    ``test case 31`` ()=
           Not(And(ctrue, cfalse)) |> ExpandCondition |> 
           should equal (Or(Not(cfalse), Not(ctrue)))
   [<Test>] member test.
    ``test case 32`` ()=
           Not(Not(And(ctrue, cfalse))) |> ExpandCondition |> 
           should equal (And(Not(Not(cfalse)), Not(Not(ctrue))))
   [<Test>] member test.
    ``test case 33`` ()=
           Not(ctrue) |> SimplifyCondition |> should equal cfalse
   [<Test>] member test.
    ``test case 34`` ()=
           Not(Not(And(ctrue, cfalse))) |> SimplifyCondition |> should equal cfalse
//Format(Simplify((Const(1.) * x) / Const(2.)))
//Format(SimplifyConstant(Const(1.) / Const(2.) + Const(3.)))
//Format(SimplifyConstant(Const(4.) / Const(2.) ))
//
//SimplifyConstant(Const(1.) / Const(2.) + Const(3.))
//let f e =
//    match e with
//    | Constant(c) -> c, Const(1.)
//    | _ -> Const(1.), e
//
//printfn "-- end -- "
