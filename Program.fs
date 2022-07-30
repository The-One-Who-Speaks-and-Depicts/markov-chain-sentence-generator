// TODO: Markov Chain Sentence Generator

open System;

let sentences = [ ["#START"; "I"; "swim"; "."]; ["#START"; "I"; "go"; "swimming"; "."] ] ;

let sentenceOutput(sentence: List<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output

let getNGram (tokenPos: int, n: int, sentence: List<string>) =
    let mutable output : List<string> = [];
    for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then output <- output @ [sentence[i]] else output <- output @ ["#END"];
    output


let splitSentenceIntoNGrams (sentence: List<string>, n: int) =
    let mutable output : List<List<string>> = [];
    for i = 0 to sentence.Length - 1 do
        output <- output @ [getNGram(i, n, sentence)];
    output

for ngram in splitSentenceIntoNGrams(sentences[0], 2) do
    printfn "%s" (String.concat " " ngram)
