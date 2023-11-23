open FSharp.Data

[<Measure>] type EUR
[<Measure>] type PLN


type Person = 
    {
        name:string;
        lastname:string;
        pesel:string option;
    }
type Customer =
    {
        mutable id: int;
        mutable person: Person option;
        mutable maxCredit: decimal<PLN>;
        mutable registrationDate: System.DateTime;
        mutable status: string;
    }

type Status = 
    static member normal : string = "norm"
    static member vip : string = "VIP"
      
      
module Utils = 
    module Currency = 
        let DecimalToPLN (x : decimal) = 
            System.Math.Round((x |> decimal), 2) * 1m<PLN>
            
        let DecimalToEUR (x : decimal) = 
            System.Math.Round((x |> decimal), 2) * 1m<EUR>

        let EUR_to_PLN(input : decimal<EUR>) =
            let result = input * 4.36m<PLN/EUR>
            result

        let PLN_to_EUR(input : decimal<PLN>) =
            let result = input * 0.23m<EUR/PLN>
            result

    module XML = 
        [<Literal>]
        let path = @"C:\Users\wdmk4\source\git\PG\JP_NET_Lab_6\MyProject\Shop\transactions.xml"
        type Transactions = XmlProvider<path>

        let GetCustomer id =
            let customers = Transactions.GetSample().Customers
            let customer = 
                customers
                |> Seq.filter(fun c -> c.Id = id)
                |> Seq.exactlyOne
            customer

        let GetTransactionsList id = 
            GetCustomer(id).Transactions

        let GetPaidTransactionsValue id =
            let mutable result : decimal<EUR> = 0.m<EUR>
            for t in GetTransactionsList(id) do
                if t.Paid then
                    result <- result + (Currency.DecimalToEUR(t.Value))
            result

        let GetUnpaidTransactionsValue id =
            let mutable result : decimal<EUR> = 0.m<EUR>
            for t in GetTransactionsList(id) do
                if t.Paid = false then
                    result <- result + (Currency.DecimalToEUR(t.Value))
            result

    module GenerateData = 
        let GetCustomerList =
            let person1 = { name = "name 1"; lastname = "lastname 1"; pesel = Some("12345678900") }
            let person2 = { name = "name 2"; lastname = "lastname 2"; pesel = Some("01234567890") }
            let person3 = { name = "name 3"; lastname = "lastname 3"; pesel = Some("00123456789") }

            let customer1 = { id = 1; person = Some(person1); maxCredit = 2m<PLN>; registrationDate = System.DateTime(2023, 11, 1); status = Status.normal }
            let customer2 = { id = 2; person = Some(person2); maxCredit = 4m<PLN>; registrationDate = System.DateTime(2023, 11, 7); status = Status.normal }
            let customer3 = { id = 3; person = Some(person3); maxCredit = 6m<PLN>; registrationDate = System.DateTime(2023, 11, 14); status = Status.vip }
            let customer4 = { id = 4; person = None ; maxCredit = 8m<PLN>; registrationDate = System.DateTime(2023, 11, 22); status = Status.normal }
            [customer1; customer2; customer3; customer4]
        
    let GetCustomer customers id = 
        let customer =
            customers
            |> Seq.filter (fun c -> c.id = id)
            |> Seq.exactlyOne
        customer
        
type Shop(customers : List<Customer>) = 
    let customers = customers
    let minimalTimeForPromotion = 7
    let minimalTransactionsForPromotion = 500m<EUR>

    member this.Promote(id: int) = 
        let customer = Utils.GetCustomer customers id
        if Utils.XML.GetPaidTransactionsValue(id) >= minimalTransactionsForPromotion 
            && minimalTimeForPromotion <= (System.DateTime.Now - customer.registrationDate).Days then
            customer.status <- Status.vip
            printfn "Customer Promoted!"
        if Utils.XML.GetPaidTransactionsValue(id) < minimalTransactionsForPromotion then
            printfn "Not enough transactions!"
        if minimalTimeForPromotion > (System.DateTime.Now - customer.registrationDate).Days then
            printfn "Not enough time!"
        
    member this.GetCustomerInfo(id: int) = 
        let customer = Utils.GetCustomer customers id
        customer

    member this.IncreaseMaxCredit(id: int, value: decimal<PLN>) = 
        let customer = Utils.GetCustomer customers id
        customer.maxCredit <- customer.maxCredit + value

    member this.GetMaxCredit(id: int) = 
        let customer = Utils.GetCustomer customers id
        customer.maxCredit

module Menu = 

    let PrintMenu() = 
        printfn "\n\t###############################"
        printfn "\tChoose option"
        printfn "\t\t1. Read customer info"
        printfn "\t\t2. Increase max credit"
        printfn "\t\t3. Check transactions"
        printfn "\t\t4. Promotion\n"

    let GetOption() =
        printfn "Choose option"
        System.Console.ReadLine() |> int

    let GetID() = 
        printfn "\nID:"
        System.Console.ReadLine() |> int


    let GetCustomerInfo(shop : Shop) = 
        let id = GetID()
        let customerInfo = shop.GetCustomerInfo(id)
        printfn "\tCustomer Info"
        printfn "\t\t ID: %d" customerInfo.id
        printfn "\t\t Status: %s" (customerInfo.registrationDate.ToShortDateString())
        printfn "\t\t maxCredit: %M PLN (%M EUR)" customerInfo.maxCredit (Utils.Currency.PLN_to_EUR(customerInfo.maxCredit))
        printfn "\t\t Status: %s" customerInfo.status
        if customerInfo.person.IsSome then
            printfn "\t\t Name: %s" customerInfo.person.Value.name
            printfn "\t\t Last name: %s" customerInfo.person.Value.lastname
            if customerInfo.person.Value.pesel.IsSome then
                printfn "\t\t Pesel: %s" customerInfo.person.Value.pesel.Value

    let IncreaseMaxCredit(shop: Shop) =
        let id = GetID()
        printfn "Value [PLN]:"
        let value = System.Console.ReadLine() |> decimal
        shop.IncreaseMaxCredit(id, Utils.Currency.DecimalToPLN(value))
        let newValue = shop.GetMaxCredit(id)
        printfn "New Value: %M PLN" newValue

    let CheckTransactions(shop : Shop) =
        let id = GetID()
        let value = Utils.XML.GetUnpaidTransactionsValue(id)
        let maxCredit = shop.GetMaxCredit(id)
        printfn "value: %M EUR maxCredit: %M EUR" value (Utils.Currency.PLN_to_EUR(maxCredit))
        if value < Utils.Currency.PLN_to_EUR(maxCredit) then
            printfn "Its okey"
        else
            printfn "Its not okey"

    let PromoteCustomer(shop : Shop) =
        let id = GetID()
        shop.Promote(id)

module Program =
    [<EntryPoint>]
    let main args =
        let shop = new Shop(Utils.GenerateData.GetCustomerList)

        while true do
            Menu.PrintMenu()
            match Menu.GetOption() with
            | 1 -> Menu.GetCustomerInfo shop
            | 2 -> Menu.IncreaseMaxCredit shop
            | 3 -> Menu.CheckTransactions shop
            | 4 -> Menu.PromoteCustomer shop
            | _ -> printfn "nothing"
        0