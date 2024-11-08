// Simple record type for a stack. We only need to track the number of elements on the
// stack, so this is really simple
type Stack = {
    depth: int
}

// Cleaning and separating the input
let sepInput (input: string) =
    input.ToCharArray()
    |> Array.toList
    |> List.filter (fun x -> x = 'a' || x = 'b')


/// Expected input is a string of as and bs, any other characters are ignored!
/// Returns a boolean indicating whether the string is a member of the a^n b^n language
let parse (input: string) =
    let ipt = sepInput input

    // Inner recursive function that works its way through the input
    let rec innerparse lst stck =
        match lst with
        | [] -> stck
        | head :: rest when head = 'a' -> innerparse rest {stck with depth = stck.depth + 1}
        | head :: rest when head = 'b' -> innerparse rest {stck with depth = stck.depth - 1}
        // Should never get here, but needed for completeness of pattern match
        | head :: rest -> innerparse rest stck
    
    let stack = innerparse ipt {depth = 0}

    match stack.depth with
    | 0 -> true
    | _ -> false

[<EntryPoint>]
let main args =
    let input = args[0]
    let result = parse input
    match result with
    | true -> printf $"String '{input}' belongs to a^n b^n\n"
    | _ -> printf $"String '{input}' doesn't belong to a^n b^n\n"
    0
