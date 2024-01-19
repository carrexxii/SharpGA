namespace FPGA

open System

[<Measure>] type e0
[<Measure>] type e1
[<Measure>] type e2
[<Measure>] type e01 = e0*e1
[<Measure>] type e02 = e0*e2
[<Measure>] type e12 = e1*e2
[<Measure>] type e012 = e0*e1*e2

module PGA2D =
    let basis = [ ""; "e0"; "e1"; "e2"; "e01"; "e02"; "e12"; "e012" ]

    let ( !<> ) (x: float<_>) =
        float x

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

    type Blade = 
        | Zero  of float
        | One   of Vec
        | Two   of Bivec
        | Three of PSS

    and Vec =
        { e0: float<e0>
          e1: float<e1>
          e2: float<e2> }
        static member Default =
            { e0 = 0.0<e0>
              e1 = 0.0<e1>
              e2 = 0.0<e2> }
        static member create a b c =
            { e0 = a * 1.0<e0>
              e1 = b * 1.0<e1>
              e2 = c * 1.0<e2> }

        member this.list = [ !<>this.e0; !<>this.e1; !<>this.e2 ]

        member this.Item = function
            | 0 -> !<>this.e0
            | 1 -> !<>this.e1
            | 2 -> !<>this.e2
            | _ -> raise (IndexOutOfRangeException ())

        override this.ToString () =
            vecStr this.list (basis |> List.skip 1 |> List.take 3)

        static member ( !* ) (a: Vec) = // Poincare Dual
            { e01 =  a.e2 * 1.0<e01/e2>
              e02 = -a.e1 * 1.0<e02/e1>
              e12 =  a.e0 * 1.0<e12/e0> }
        static member ( !** ) (a: Vec) = !*a // Hodge dual

        static member ( ~~ ) (a: Vec) = !*(!*a) // Inverse

        static member ( + ) (a: Vec, b: Vec) =
            { e0 = a.e0 + b.e0
              e1 = a.e1 + b.e1
              e2 = a.e2 + b.e2 }
        
        static member ( * ) (a: Vec, s: float) =
            { e0 = a.e0 * s
              e1 = a.e1 * s
              e2 = a.e2 * s }
        static member ( * ) (s: float, a: Vec) = a * s

        static member ( .^. ) (a: Vec, s: float) = s * a
        static member ( .^. ) (s: float, a: Vec) = s * a
        static member ( .^. ) (a: Vec, b: Vec) =
            { e01 = a.e0*b.e1 - a.e1*b.e0
              e02 = a.e0*b.e2 - a.e2*b.e0
              e12 = a.e1*b.e2 - a.e2*b.e1 }
        static member ( .^. ) (a: Vec, B: Bivec) =
            { e012 = a.e0*B.e12 - a.e1*B.e02 + a.e2*B.e01 }
        
        static member op_Amp (a: Vec, b: Vec) =
            ()

    and Bivec =
        { e01: float<e01>
          e02: float<e02>
          e12: float<e12> }
        static member Default =
            { e01 = 0.0<e01>
              e02 = 0.0<e02>
              e12 = 0.0<e12> }
        static member create a b c =
            { e01 = a*1.0<e01>
              e02 = b*1.0<e02>
              e12 = c*1.0<e12> }

        member this.list = [ !<>this.e01; !<>this.e02; !<>this.e12 ]

        member this.Item = function
            | 0 -> !<>this.e01
            | 1 -> !<>this.e02
            | 2 -> !<>this.e12
            | _ -> raise (IndexOutOfRangeException ())

        override this.ToString () =
            vecStr this.list (basis |> List.skip 4 |> List.take 3)

        static member ( !* ) (a: Bivec): Vec = // Poincare Dual
            { e0 = a.e12 * 1.0<e0/e12>
              e1 = a.e02 * 1.0<e1/e02>
              e2 = a.e01 * 1.0<e2/e01> }
        static member ( !** ) (a: Vec) = !*a // Hodge dual

        static member ( ~~ ) (a: Vec) = !*(!*a) // Inverse

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

        static member ( + ) (I1, I2) =
            { e012 = I1.e012 + I2.e012 }

        static member ( * ) (I: PSS, s: float) =
            { e012 = I.e012 * s }
        static member ( * ) (s: float, I: PSS) = I * s

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

    let vec   = Vec.create
    let bivec = Bivec.create
    let pss   = PSS.create
    let mvec  = MultiVec.create
