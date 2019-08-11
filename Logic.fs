module Logic

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
        let discount =upcDiscount  product.UPC  product.Price    
        let tax = calcTax (product.Price - discount)
        let otherDiscount = calcDiscount (product.Price - discount)        
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
                        
//let PriceCalculator tax discount UpcDiscount       







