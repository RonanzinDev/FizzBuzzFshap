namespace FizzBuzz
open System
open System.ComponentModel.DataAnnotations

[<RequireQualifiedAccess>]
module Option =
    let fromTryTuple = function
        | false, _ -> None
        | true, x -> Some x
    
[<RequireQualifiedAccess>]
module Result =
    let fromOption errorValue = function
        | Some x -> Ok x
        | None -> Error errorValue
      

module Parser =
   let tryParse (input : string) =
        Int32.TryParse input
        |> Option.fromTryTuple


module Validator =
    type ValidNumber = private ValidNumber of int
    module ValidNumber =
        let isValidNumber number =
            match 1 <= number && number <= 4000 with
            | false -> None
            | true -> Some <| ValidNumber number
        let value (ValidNumber number) = number
              
module FizzBuzz =
    open Validator
    let getFizzBuzzString validNumber =
            [1.. ValidNumber.value validNumber]
            |> List.map(fun n -> (n, n % 3, n % 5))
            |> List.map(function
                | (_,0,0) -> "FizzBuzz"
                | (_, 0,_) -> "Fizz"
                | (_, _, 0) -> "Buzz"
                | (n, _, _) -> string n)
            |> String.concat "\n"
       
module Domain =
    open Validator
    type ParseNumber = string -> int option
    type ValidateNumber = int -> ValidNumber option
    type GetFizzBuzzString = ValidNumber -> string
    type ParseError = NotAnumber of string
    type ValidatorError = InvalidNumber of int
    type Error =
        | ParseError of ParseError
        | ValidatorError of ValidatorError
    type ExecuteFizzBuzzWorkFlow = string -> Result<string, Error>
    
    let execute
        (parseNumber: ParseNumber)
        (validateNumber: ValidateNumber) 
        (getFizzBuzzString: GetFizzBuzzString) : ExecuteFizzBuzzWorkFlow =
        let parseNumber input =
            input
            |> parseNumber
            |> Result.fromOption (NotAnumber input)
            |> Result.mapError ParseError
            
        let validateNumber number =
            number |> validateNumber |> Result.fromOption (InvalidNumber number) |> Result.mapError ValidatorError
        
        fun input ->
            input |> parseNumber |> Result.bind validateNumber |> Result.map getFizzBuzzString
            
module Application =
    open Domain
    type Input = unit -> string
    type Output = string -> unit
    let execute =
        Domain.execute
            Parser.tryParse
            Validator.ValidNumber.isValidNumber
            FizzBuzz.getFizzBuzzString
            
    let application (input: Input) (output: Output) =
        fun () -> 
            output "Entre com um numero entre 1 e 4000"
            input ()
            |> execute
            |> function
                | Ok s ->
                    sprintf "Aqui está a saida:\n%s" s
                    |> output
                | Error (ParseError (NotAnumber s)) ->
                    sprintf "%s não é um inteiro. " s
                    |> output
                | Error (ValidatorError (InvalidNumber num)) ->
                    sprintf "Você digitou %i. Por favor entre com um numero de 1 a 4000." num
                    |> output