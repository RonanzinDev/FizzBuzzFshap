open System
[<EntryPoint>]
let main argv =
    printf "Digite um numero de 1 a 4000: "
    let input = Console.ReadLine()
    match Int32.TryParse input with
    | false, _ -> printfn "%s Não é um numero valido" input
    | true, number ->
        match 1 <= number && number <= 4000 with
        | false -> printfn "Você digitou %i. Por favor entre com um numero valido" number
        | true ->
            [1..number]
            |> List.map(fun n -> (n, n % 3, n % 5))
            |> List.map(function
                | (_,0,0) -> "FizzBuzz"
                | (_, 0,_) -> "Fizz"
                | (_, _, 0) -> "Buzz"
                | (n, _, _) -> string n)
            |> String.concat "\n"
            |> printfn "Here is the output:\n%s"
    0