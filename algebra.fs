namespace PGA

[<Measure>] type e0
[<Measure>] type e1
[<Measure>] type e2
[<Measure>] type e01 = e0*e1
[<Measure>] type e02 = e0*e2
[<Measure>] type e12 = e1*e2
[<Measure>] type e012 = e0*e1*e2

module PGA2D =
    type Vec =
        { e0: float<e0>
          e1: float<e1>
          e2: float<e2> }

        static member create a b c =
            { e0 = a * 1.0<e0>
              e1 = b * 1.0<e1>
              e2 = c * 1.0<e2> }

        static member ( + ) (l, r) =
            { e0 = l.e0 + r.e0
              e1 = l.e1 + r.e1
              e2 = l.e2 + r.e2 }

        override this.ToString () =
            $"Vector: ({this.e0}e0, {this.e1}e1, {this.e2}e2)"

    and Bivec =
        { e01: float<e01>
          e02: float<e02>
          e12: float<e12> }

        static member create a b c =
            { e01 = a*1.0<e01>
              e02 = b*1.0<e02>
              e12 = c*1.0<e12> }

        static member ( + ) (l, r) =
            { e01 = l.e01 + r.e01
              e02 = l.e02 + r.e02
              e12 = l.e12 + r.e12 }

        override this.ToString () =
            $"BiVector: ({this.e01}e01, {this.e02}e02, {this.e12}e12)"

    and PSS =
        { e012: float<e012> }

        static member create a =
            { e012 = a*1.0<e012> }

        static member ( + ) (l, r) =
            { e012 = l.e012 + r.e012 }

        override this.ToString () =
            $"PseudoScalar: {this.e012}e012"

    and MultiVec () =
        member this.scalar: float option = None
        member this.vec   : Vec   option = None
        member this.bivec : Bivec option = None
        member this.pss   : PSS   option = None
        member this.blades = Set<Blade>

    and Blade = 
        | Zero  of float
        | One   of Vec
        | Two   of Bivec
        | Three of PSS

    let vec   = Vec.create
    let bivec = Bivec.create
    let pss   = PSS.create
