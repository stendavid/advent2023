open System.IO
open System.Text.RegularExpressions

let tryParseText (s: string) : int option =
    match s.ToLower() with
    | "one" -> Some 1
    | "two" -> Some 2
    | "three" -> Some 3
    | "four" -> Some 4
    | "five" -> Some 5
    | "six" -> Some 6
    | "seven" -> Some 7
    | "eight" -> Some 8
    | "nine" -> Some 9
    | _ ->
        // Check if the string represents a numeric digit
        match System.Int32.TryParse(s) with
        | true, num -> Some num
        | _ -> None

let findAllDigitsOrTexts (text: string) : int list =
    let digitPattern = @"[1-9]"
    let textPattern = @"one|two|three|four|five|six|seven|eight|nine"

    let regex = new Regex($"({digitPattern}|{textPattern})", RegexOptions.IgnoreCase)

    regex.Matches(text)
        |> Seq.map (fun matchResult -> matchResult.Value |> tryParseText)
        |> Seq.choose id
        |> List.ofSeq

let extractFirstAndLastDigits (filePath: string) : unit =
    let mutable totalSum = 0

    try
        use streamReader = File.OpenText(filePath)
        while not streamReader.EndOfStream do
            let line = streamReader.ReadLine()

            let digits = findAllDigitsOrTexts line

            let firstDigit, lastDigit, value =
                match digits with
                | [] -> None, None, None
                | _ ->
                    let d1 = List.head digits
                    let d2 = List.last digits
                    Some d1, Some d2, Some (int (string d1 + string d2))

            //printfn "Line: %s, Digits: %A, First Digit: %A, Last Digit: %A, Value: %A" line digits firstDigit lastDigit value

            totalSum <- totalSum +
                match value with
                | Some digit -> digit
                | None -> 0

        printfn "\nTotal Sum of Numbers for All Lines: %d" totalSum

    with
    | :? System.IO.FileNotFoundException as ex ->
        printfn "Error: %s" ex.Message
    | :? System.IO.IOException as ex ->
        printfn "Error: %s" ex.Message

// Example usage
extractFirstAndLastDigits "input"
