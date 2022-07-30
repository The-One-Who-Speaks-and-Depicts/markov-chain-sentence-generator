// TODO: Markov Chain Sentence Generator

open System;

let sentences = [ ["#START"; "I"; "swim"; "."; "#END"]; ["#START"; "I"; "go"; "swimming"; "."; "#END"] ] ;

let sentenceOutput(sentence: List<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output



for sentence in sentences do
    printfn "%s" (sentenceOutput(sentence));

let rec nGramCollect (tokenPos: int, n: int, sentence: List<string>) =
    let mutable output = sentence[tokenPos];
    if n > 1 then String.concat " " [output; nGramCollect(tokenPos + 1, n - 1, sentence)] else output;

printfn "%s" (nGramCollect(1, 2, sentences[0]));
