namespace PGA

open Microsoft.FSharp.Collections

[<Measure>] type e0
[<Measure>] type e1
[<Measure>] type e2
[<Measure>] type e01 = e0*e1
[<Measure>] type e02 = e0*e2
[<Measure>] type e12 = e1*e2
[<Measure>] type e012 = e0*e1*e2


module PGA2D =
    let basis = [ ""; "e0"; "e1"; "e2"; "e01"; "e02"; "e12"; "e012" ]

    let ( ! ) (x: float<_>) =
        float x

    let rec vecStr elems suffixes =
        let plus = function
            | _, [], s  -> s
            | 0.0, _, _ -> ""
            | _, _, s   -> s + " + "
        let elemStr x =
            if x <> 0.0<_>
            then $"{x}"
            else ""
        match elems, suffixes with
        | _, []
        | [], _ -> ""
        | x::xs, s::ss -> elemStr x + plus (x, xs, s) + vecStr xs ss

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

        static member ( + ) (l, r) =
            { e0 = l.e0 + r.e0
              e1 = l.e1 + r.e1
              e2 = l.e2 + r.e2 }

        member this.list =
            [ !this.e0; !this.e1; !this.e2 ]

        override this.ToString () =
            vecStr this.list (basis |> List.skip 1 |> List.take 3)

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

        static member ( + ) (l, r) =
            { e01 = l.e01 + r.e01
              e02 = l.e02 + r.e02
              e12 = l.e12 + r.e12 }

        member this.list =
            [ !this.e01; !this.e02; !this.e12 ]

        override this.ToString () =
            vecStr this.list (basis |> List.skip 4 |> List.take 3)

    and PSS =
        { e012: float<e012> }
        static member Default =
            { e012 = 0.0<e012> }

        static member create a =
            { e012 = a*1.0<e012> }

        static member ( + ) (l, r) =
            { e012 = l.e012 + r.e012 }

        member this.list =
            [ !this.e012 ]

        override this.ToString () =
            vecStr this.list (basis |> List.skip 7 |> List.take 1)

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

        member this.list =
            [ this.scalar ] @ this.vec.list @ this.bivec.list @ this.pss.list

        override this.ToString () =
            vecStr this.list basis

    let vec   = Vec.create
    let bivec = Bivec.create
    let pss   = PSS.create
    let mvec  = MultiVec.create
