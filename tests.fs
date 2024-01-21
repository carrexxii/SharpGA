open Expecto

open FPGA

let test name case expect str =
    testCase name <| fun () ->
        Expect.equal case expect str
        Expect.equal $"{case}" str str

module TestR200 =
    open R200

    [<Tests>]
    let testVecs =
        let a = vec  0  0
        let b = vec  1  1
        let c = vec -1 -1
        let d = vec  3 -7
        let e = vec -2 10

        testList "Vector tests" [
            testList "Dual" [
                test $"a* = ({a})*" !*a (vec   0  0) "0"
                test $"b* = ({b})*" !*b (vec  -1  1) "-e1 + e2"
                test $"c* = ({c})*" !*c (vec   1 -1) "e1 - e2"
                test $"d* = ({d})*" !*d (vec   7  3) "7e1 + 3e2"
                test $"e* = ({e})*" !*e (vec -10 -2) "-10e1 - 2e2"
            ]
        ]

module TestR201 =
    open R201

    // https://bivector.net/tools.html?p=2&q=0&r=1
    [<Tests>]
    let testVecs =
        let a = vec  2  3  1
        let b = vec  5  6  4
        let c = vec  1  0  0
        let d = vec  0 -1 -1
        let e = vec -2 -3 -1

        let A = bivec 1 2 3
        let B = bivec 4 5 6
        let C = bivec 0 1 0
        let D = bivec -1 0 -1
        let E = bivec -1 -2 -3

        testList "Vector tests" [
            testList "Dual" [
                test $"a* = ({a})*" !*a (bivec 3 -2  1) "3e01 - 2e02 + e12"
                test $"b* = ({b})*" !*b (bivec 6 -5  4) "6e01 - 5e02 + 4e12"
                test $"c* = ({c})*" !*c (bivec 0 -1  0) "-e02"
                test $"d* = ({d})*" !*d (bivec -1 0 -1) "-e01 - e12"
                test $"e* = ({e})*" !*e (bivec -3 2 -1) "-3e01 + 2e02 - e12"
            ]
            testList "Outer Product" [
                test $"a∧b = ({a})∧({b})" (a .^. b) (bivec -3 -6 -3) "-3e01 - 6e02 - 3e12"
                test $"a∧c = ({a})∧({c})" (a .^. c) (bivec  1  0 -3) "e01 - 3e12"
                test $"a∧d = ({a})∧({d})" (a .^. d) (bivec  2  2 -2) "2e01 + 2e02 - 2e12"
                test $"a∧e = ({a})∧({e})" (a .^. e) (bivec  0  0  0) "0"
            ]
            testList "Regressive Product" [
                test $"A∨B = ({A}) ∨ ({A})" (A .&. A) (vec  0 0  0) "0"
                test $"A∨B = ({A}) ∨ ({B})" (A .&. B) (vec  6 3  3) "6e1 + 3e2 + 3e0"
                test $"A∨C = ({A}) ∨ ({C})" (A .&. C) (vec  0 3 -1) "3e2 - e0"
                test $"A∨D = ({A}) ∨ ({D})" (A .&. D) (vec -2 2 -2) "-2e1 + 2e2 - 2e0"
                test $"A∨E = ({A}) ∨ ({E})" (A .&. E) (vec  0 0  0) "0"
            ]
        ]

[<EntryPoint>]
let runTests args =
    runTestsInAssemblyWithCLIArgs [] [||]
