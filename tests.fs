open Expecto

open FPGA.PGA2D

// https://bivector.net/tools.html?p=2&q=0&r=1
[<Tests>]
let testVecs =
    let a = vec 1 2 3
    let b = vec 4 5 6
    let c = vec 0 1 0
    let d = vec -1 0 -1
    testList "Vector tests" [
        testList "Outer Product" [
            testCase "a∧b" <| fun () ->
                let case   = a .^. b
                let expect = (bivec -3 -6 -3)
                Expect.equal case expect ""
                Expect.equal $"{case}" "-3e01 + -6e02 + -3e12" ""
            testCase "a∧c" <| fun () ->
                let case   = a .^. c
                let expect = (bivec 1 0 -3)
                Expect.equal case expect ""
                Expect.equal $"{case}" "e01 + -3e12" ""
            testCase "a∧d" <| fun () ->
                let case   = a .^. d
                let expect = (bivec 2 2 -2)
                Expect.equal case expect ""
                Expect.equal $"{case}" "2e01 + 2e02 + -2e12" ""
        ]
    ]

[<EntryPoint>]
let runTests args =
    runTestsInAssemblyWithCLIArgs [] [||]
