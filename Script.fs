open System
//build a pipeline of operations ie (discount , tax , discount) or (tax, discount, discount) etc
type Product = {Name:string;  UPC:string;Price:decimal;} 
type Invoice = {Tax:decimal; Discount:decimal; Total:decimal}//discount should be a list of its own type
let DefaultRounding (x:decimal) =     
    Math.Round(x,2)
let calcTax tax price =
    price * tax |> DefaultRounding

let calcDiscount discount price  =
    price * discount |> DefaultRounding
    
let UpcDiscount upc discount pUPC price =
    if upc = pUPC then
        price * discount |> DefaultRounding
    else
        0M
 

let calcTotal calcTax calcDiscount upcDiscount posttax upcPostTax product =       
    match (posttax,upcPostTax) with
    | true,true ->
        let discount = calcDiscount  product.Price    
        let otherDiscount = upcDiscount product.UPC product.Price
        let tax =calcTax (product.Price - (discount+otherDiscount))
        let total = product.Price + tax - (discount + otherDiscount)    
        {Tax=tax;Discount= (discount+otherDiscount);Total=total}
    | true,false ->
        let discount = calcDiscount  product.Price    
        let tax = calcTax (product.Price - discount)
        let otherDiscount = upcDiscount product.UPC (product.Price - tax)        
        let total = product.Price + tax - (discount + otherDiscount)    
        {Tax=tax;Discount= (discount+otherDiscount);Total=total}
        
    | false,true ->
        let otherDiscount = upcDiscount product.UPC (product.Price )        
        let tax = calcTax (product.Price - otherDiscount)
        let discount = calcDiscount  (product.Price - tax)    
        let total = product.Price + tax - (discount + otherDiscount)    
        {Tax=tax;Discount= (discount+otherDiscount);Total=total}
    | false, false -> 
        let tax = calcTax product.Price
        let discount = calcDiscount  product.Price    
        let otherDiscount = upcDiscount product.UPC product.Price         
        let total = product.Price + tax - (discount + otherDiscount)    
        {Tax=tax;Discount= (discount+otherDiscount);Total=total}
           
    
let Display total =
    printfn "price %.2f" total.Total
    if total.Discount > 0M then
        printfn "%.2f amount which was deducted" total.Discount
        
        
        
 (**********************************************************************)       
let r1Tax = calcTax 0.20M
let r1Discount = calcDiscount 0M
let r1Total = calcTotal r1Tax r1Discount (fun _ _-> 0M)
let r1 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
let t1 = r1Total false false r1
if t1.Total <> 24.30M  then failwith "r1 failed"
Display t1 

let r2Tax = calcTax 0.20M
let r2Discount = calcDiscount 0.15M
let r2Total = calcTotal r2Tax r2Discount (fun _ _ -> 0M)
let r2 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
let t2 = r2Total false false r2
if t2.Tax <> 4.05M || Math.Round(t2.Discount,2) <> 3.04M || Math.Round(t2.Total,2) <> 21.26M   then failwith "r2 failed"
Display t2


let r4Tax = calcTax 0.20M
let r4Discount = calcDiscount 0.15M
let r4Upc = UpcDiscount "12345" 0.07M

let r4Total = calcTotal r4Tax r4Discount r4Upc
let r4 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
let t4 = r4Total false false r4
if t4.Tax <> 4.05M || Math.Round(t4.Discount,2) <> 4.46M || Math.Round(t4.Total,2) <> 19.84M   then failwith "r4 failed"
Display t4

let r5Tax = calcTax 0.20M
let r5Discount = calcDiscount 0.15M
let r5Upc = UpcDiscount "12345" 0.07M

let r5Total = calcTotal r5Tax r5Discount r5Upc
let r5 = {Name = "The Little Prince"; UPC = "12345"; Price = 20.25M;}
let t5 = r5Total true false r5
//if t5.Tax <> 4.05M || Math.Round(t5.Discount,2) <> 4.46M || Math.Round(t5.Total,2) <> 19.84M   then failwith "r5 failed"
Display t5
