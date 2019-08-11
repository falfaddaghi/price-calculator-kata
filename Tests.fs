module Tests

open System
open Xunit
open Logic
open Swensen.Unquote.Assertions 

let debug () = 
    if not(System.Diagnostics.Debugger.IsAttached) then
      printfn "Please attach a debugger, PID: %d" (System.Diagnostics.Process.GetCurrentProcess().Id)
    while not(System.Diagnostics.Debugger.IsAttached) do
      System.Threading.Thread.Sleep(100)
    System.Diagnostics.Debugger.Break()

[<Fact>]
let ``req 1 `` () =
    let r1Tax = calcTax 0.20M
    let r1Discount = calcDiscount 0M
    let r1Total = calcTotal r1Tax r1Discount (fun _ _-> 0M)
    let r1 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
    let t1 = r1Total false false r1
    if t1.Total <> 24.30M  then failwith "r1 failed"
    Display t1 

[<Fact>]
let ``req 2 `` () =
    let r2Tax = calcTax 0.20M
    let r2Discount = calcDiscount 0.15M
    let r2Total = calcTotal r2Tax r2Discount (fun _ _ -> 0M)
    let r2 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
    let t2 = r2Total false false r2
    if t2.Tax <> 4.05M || Math.Round(t2.Discount,2) <> 3.04M || Math.Round(t2.Total,2) <> 21.26M   then failwith "r2 failed"

[<Fact>]
let ``req 3 `` () =
    ()
[<Fact>]
let ``req 4 `` () =
    let r4Tax = calcTax 0.20M
    let r4Discount = calcDiscount 0.15M
    let r4Upc = UpcDiscount "12345" 0.07M

    let r4Total = calcTotal r4Tax r4Discount r4Upc
    let r4 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
    let t4 = r4Total false false r4
    test <@ t4.Tax = 4.05M && t4.Discount = 4.46M && t4.Total = 19.84M   @>


[<Fact>]
let ``req 5 `` () =        
    
    let r5Tax = calcTax 0.20M
    let r5Discount = calcDiscount 0.15M
    let r5Upc = UpcDiscount "12345" 0.07M

    let r5Total = calcTotal r5Tax r5Discount r5Upc
    let r5 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
    let t5 = r5Total true false r5
    test <@ t5.Tax = 3.77M  && t5.Discount = 4.24M && t5.Total = 19.78M   @>      
      

[<Fact>]
let ``req 6 `` () =
    ()