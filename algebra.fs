namespace FPGA

open System

[<AutoOpen>]
module Algebra =
    let vecStr elems suffixes =
        let plus = function
            | 0.0, _ , _ -> ""
            | _  , [], b -> b
            | _  , _ , b -> $"{b} + "
        let coeff = function
            | 0.0
            | 1.0  -> ""
            | -1.0 -> "-"
            | x    -> $"{x}"
        let rec loop = function
            | _, []
            | [], _ -> ""
            | x::xs, b::bs -> coeff x + plus (x, xs, b) + loop (xs, bs)
        loop (elems, suffixes)
        |> _.Replace("+ -", "- ")
        |> _.TrimEnd([|' '; '+'|])
        |> (fun s -> if s = "" then "0" else s)

module R200 =
    let basis = [ ""; "e1"; "e2"; "e12" ]

    [<RequireQualifiedAccess>]
    type Blades =
        | Zero of float
        | One  of Vec
        | Two  of Bivec

    and Vec =
        { e1: float
          e2: float }
        static member Default =
            { e1 = 0.0
              e2 = 0.0 }
        static member create e1 e2 =
            { e1 = e1
              e2 = e2 }

        member this.list = [ this.e1; this.e2 ]

        override this.ToString () =
            vecStr this.list (basis |> List.skip 1 |> List.take 2)

        // a* = aI
        static member ( !* ) a =
            { e1 = -a.e2
              e2 =  a.e1 }

    and Bivec =
        { e12: float }
        static member Default =
            { e12 = 0.0 }
        static member create e12 =
            { e12 = e12 }
    
    [<AutoOpen>]
    module Helpers =
        let vec   = Vec.create
        let bivec = Bivec.create

[<Measure>] type e0
[<Measure>] type e1
[<Measure>] type e2
[<Measure>] type e01 = e0*e1
[<Measure>] type e02 = e0*e2
[<Measure>] type e12 = e1*e2
[<Measure>] type e012 = e0*e1*e2

module R201 =
    let basis = [ ""; "e1"; "e2"; "e0"; "e01"; "e02"; "e12"; "e012" ]

    let ( !<> ) (x: float<_>) =
        float x

    type Point =
        { x: float
          y: float }
        static member Default =
            { x = 0.0
              y = 0.0 }
        static member create x y =
            { x = x
              y = y }
        
        member this.tuple = this.x, this.y

        override this.ToString () =
            $"{this.x}, {this.y}"

        static member ( + ) (p1, p2) =
            { x = p1.x + p2.x
              y = p1.y + p2.y }

        static member ( - ) (p1, p2) =
            { x = p1.x - p2.x
              y = p1.y - p2.y }

        static member ( * ) (p, s) =
            { x = p.x * s
              y = p.y * s }

        static member ( / ) (p, s) =
            { x = p.x / s
              y = p.y / s }

    type Blade = 
        | Zero  of float
        | One   of Vec
        | Two   of Bivec
        | Three of PSS

    and Vec =
        { e1: float<e1>
          e2: float<e2>
          e0: float<e0> }
        static member Default =
            { e1 = 0.0<e1>
              e2 = 0.0<e2>
              e0 = 0.0<e0> }
        static member create a b c =
            { e1 = a * 1.0<e1>
              e2 = b * 1.0<e2>
              e0 = c * 1.0<e0> }

        member this.list  = [ !<>this.e1; !<>this.e2; !<>this.e0 ]
        member this.tuple = !<>this.e1, !<>this.e2, !<>this.e0
        member this.mag =
            let x, y, w = this.tuple
            sqrt (x**2 + y**2 + w**2)
        member this.normalized =
            { e1 = this.e1 / this.mag
              e2 = this.e2 / this.mag
              e0 = this.e0 / this.mag }

        member this.Item = function
            | 0 -> !<>this.e1
            | 1 -> !<>this.e2
            | 2 -> !<>this.e0
            | _ -> raise (IndexOutOfRangeException ())

        // l = ae1 + be2 + ce0
        // 0 = ax + by + c
        // y = -(a/b)x - c/b = -(e1/e2)x - e0/e2
        member this.points (?midX: float) =
            let a, b, c = this.tuple
            let a, b = (b, -a)
            let x  = match midX with None -> this.mag | Some x -> x + this.mag
            let m  = -(a/b)
            let yi = -(c/b)
            if x = 0.0 then
                printfn $"{this}"
                Point.create  x  10.0,
                Point.create -x -10.0
            else
                Point.create  x ( m*x + yi),
                Point.create -x (-m*x + yi)

        override this.ToString () =
            vecStr this.list (basis |> List.skip 1 |> List.take 3)

        // Poincare Dual -> *
        // A* = A ⌋ I^-1
        static member ( !* ) (a: Vec) =
            { e01 =  a.e2 * 1.0<e01/e2>
              e02 = -a.e1 * 1.0<e02/e1>
              e12 =  a.e0 * 1.0<e12/e0> }

        // Hodge dual -> ★
        static member ( !** ) (a: Vec) = !*a

        // Inverse -> -*
        static member ( ~~ ) (a: Vec) = !*(!*a)

        static member ( + ) (a: Vec, b: Vec) =
            { e1 = a.e1 + b.e1
              e2 = a.e2 + b.e2
              e0 = a.e0 + b.e0 }
        
        static member ( * ) (a: Vec, s: float) =
            { e1 = a.e1 * s
              e2 = a.e2 * s
              e0 = a.e0 * s }
        static member ( * ) (s: float, a: Vec) = a * s

        static member ( / ) (a: Vec, s: float) =
            { e1 = a.e1 / s
              e2 = a.e2 / s
              e0 = a.e0 / s }

        // Outer Product / Meet
        // a ∧ b = (a0b1 - a1b0)e01 + (a0b2 - a2b0)e02 + (a1b2 - a2b1)e12
        static member ( .^. ) (a: Vec, s: float) = s * a
        static member ( .^. ) (s: float, a: Vec) = s * a
        static member ( .^. ) (a: Vec, b: Vec) =
            { e01 = a.e0*b.e1 - a.e1*b.e0
              e02 = a.e0*b.e2 - a.e2*b.e0
              e12 = a.e1*b.e2 - a.e2*b.e1 }
        static member ( .^. ) (a: Vec, B: Bivec) =
            { e012 = a.e0*B.e12 - a.e1*B.e02 + a.e2*B.e01 }

        // Regressive Product / Join
        // a ∨ b = ★-1(★a ∧ ★b)
        static member op_Amp (a: Vec, b: Bivec): float =
            !**((!**a) .^. (!**b))

    and Bivec =
        { e01: float<e01>
          e02: float<e02>
          e12: float<e12> }
        static member Default =
            { e01 = 0.0<e01>
              e02 = 0.0<e02>
              e12 = 0.0<e12> }
        static member create a b c =
            { e01 = a * 1.0<e01>
              e02 = b * 1.0<e02>
              e12 = c * 1.0<e12> }

        member this.list  = [ !<>this.e01; !<>this.e02; !<>this.e12 ]
        member this.tuple = !<>this.e01, !<>this.e02, !<>this.e12
        member this.point = Point.create !<>(this.e01 / this.e12) !<>(this.e02 / this.e12)

        member this.Item = function
            | 0 -> !<>this.e01
            | 1 -> !<>this.e02
            | 2 -> !<>this.e12
            | _ -> raise (IndexOutOfRangeException ())

        override this.ToString () =
            vecStr this.list (basis |> List.skip 4 |> List.take 3)

        static member ( !* ) (a: Bivec): Vec = // Poincare Dual -> *
            { e1 = a.e02 * 1.0<e1/e02>
              e2 = a.e01 * 1.0<e2/e01>
              e0 = a.e12 * 1.0<e0/e12> }
        static member ( !** ) (a: Bivec) = !*a // Hodge dual -> ★
        static member ( ~~ ) (a: Bivec) = !*(!*a) // Inverse -> -*

        static member ( + ) (A: Bivec, B: Bivec) =
            { e01 = A.e01 + B.e01
              e02 = A.e02 + B.e02
              e12 = A.e12 + B.e12 }
        
        static member ( * ) (A: Bivec, s: float) =
            { e01 = A.e01 * s
              e02 = A.e02 * s
              e12 = A.e12 * s }
        static member ( * ) (s: float, A: Bivec) = A * s

        static member ( .^. ) (B: Bivec, a: Vec) = a .^. B

        static member ( .&. ) (A: Bivec, B: Bivec) =
            !**((!**A) .^. (!**B))

    and PSS =
        { e012: float<e012> }
        static member Default =
            { e012 = 0.0<e012> }
        static member create a =
            { e012 = a*1.0<e012> }

        member this.list =
            [ !<>this.e012 ]

        override this.ToString () =
            vecStr this.list (basis |> List.skip 7 |> List.take 1)

        static member ( !* ) (I: PSS) = !<>I.e012

        static member ( !** ) (I: PSS) = !*I

        static member ( + ) (I1: PSS, I2: PSS) =
            { e012 = I1.e012 + I2.e012 }

        static member ( * ) (I: PSS, s: float) =
            { e012 = I.e012 * s }
        static member ( * ) (s: float, I: PSS) = I * s

        static member op_Amp (I1: PSS, I2: PSS) =
            { e012 = I1.e012 * I2.e012 / 1.0<e012> }

    and MultiVec =
        { scalar: float
          vec   : Vec
          bivec : Bivec
          pss   : PSS }
        static member Default =
            { scalar = 0.0
              vec    = Vec.Default
              bivec  = Bivec.Default
              pss    = PSS.Default }
        
        static member create scalar vec bivec pss =
            { scalar = scalar
              vec    = vec
              bivec  = bivec
              pss    = pss }

        member this.list = [ this.scalar ] @ this.vec.list @ this.bivec.list @ this.pss.list

        override this.ToString () =
            vecStr this.list basis

    [<AutoOpen>]
    type Helpers =
        static member vec   = Vec.create
        static member bivec = Bivec.create
        static member pss   = PSS.create
        static member mvec  = MultiVec.create
        static member point = Point.create

        static member midpoint (A: Bivec) (B: Bivec) =
            (A.point + B.point) / 2.0
