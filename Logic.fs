module Logic

open System
//build a pipeline of operations ie (discount , tax , discount) or (tax, discount, discount) etc
type UPC = string
type Product = {Name:string;  UPC:UPC;Price:decimal;} 
type GeneralDiscount = {Percent:decimal; Amount:decimal;PerTax:bool}
type UpcDiscount = {UPC:UPC list; Percent:decimal;PreTax:bool}
type Discount =
    | GeneralDiscount of GeneralDiscount
    | UpcDiscount of UpcDiscount
type Invoice = {Tax:decimal; Discount:Discount; Total:decimal}//discount should be a list of its own type
 
let DefaultRounding (x:decimal) =     
    Math.Round(x,2)
let calcTax tax price =
    price * tax |> DefaultRounding

let calcDiscount (discount:GeneralDiscount) price  =
    price * discount.Percent |> DefaultRounding
    
let UpcDiscount discount pUPC price =
    discount.UPC
    |> List.tryFind (fun x-> x = pUPC)
    |> Option.map(fun _ -> price * discount.Percent |> DefaultRounding)
    |> Option.defaultValue 0M
    
    
    
let Dis (discount:Discount) (product:Product) =
    match discount with
    | GeneralDiscount d  -> calcDiscount d product.Price
    |UpcDiscount u  -> UpcDiscount u product.UPC product.Price
    
        

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







