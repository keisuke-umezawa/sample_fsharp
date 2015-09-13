type Client =
    { Name : string
      Income : int
      YearsInJob : int
      UsesCreditCard : bool
      CriminalRecord : bool }

type QueryInfomation =
    { Title : string
      Check : Client -> bool
      Positive : Decision
      Negative : Decision }
and Decision = 
    | Result of string
    | Query of QueryInfomation

let rec tree =
    Query({Title = "More than $40k"
           Check = (fun client -> client.Income > 40000)
           Positive = hasCrimanalRecord; Negative = isMoreThanOneYearsInJob })
and hasCrimanalRecord =
    Query({Title = "Has criminal record"
           Check = (fun client -> client.CriminalRecord)
           Positive = Result("NO"); Negative = Result("Yes") })
and isMoreThanOneYearsInJob =
    Query({Title = "Years in job"
           Check = (fun client -> client.YearsInJob > 1)
           Positive = Result("Yes"); Negative = usesCreditCard })
and usesCreditCard =
    Query({Title = "uses credit card"
           Check = (fun client -> client.UsesCreditCard)
           Positive = Result("Yes"); Negative = Result("NO") })

let rec testClientTree client tree =
    match tree with
    | Result(message) ->
        printfn " OFFER ALOAN : %s" message
    | Query(infomation) ->
        let result, case =
            if (infomation.Check(client)) then "yes", infomation.Positive
            else "no", infomation.Negative
        printfn " - %s? %s" infomation.Title result
        testClientTree client case

let john = 
    { Name = "John Doe"
      Income = 40000
      YearsInJob = 1
      UsesCreditCard = true
      CriminalRecord = false }
    
testClientTree john tree
