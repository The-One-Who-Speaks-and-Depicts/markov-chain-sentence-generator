// TODO: Markov Chain Sentence Generator

open System;
open System.Linq;


let sentences = [ ["#START"; "I"; "swim"; "."]; ["#START"; "I"; "go"; "swimming"; "."] ] ;

let SentenceOutput(sentence: List<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output

let GetNGram (tokenPos: int, n: int, sentence: List<string>) =
    let mutable output : List<string> = [];
    for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then output <- output @ [sentence[i]] else output <- output @ ["#END"];
    output


let CollectSubchains (sentences: List<List<string>>, n: int) =
    let mutable output : List<List<string>> = [];
    for sentence in sentences do
        for i = 0 to sentence.Length - 1 do
            output <- output @ [GetNGram(i, n, sentence)];
    output

type Chained =
    {mutable value: List<string>;
    mutable amount: int;
    mutable probability: float;}

type Link =
    {mutable joined: List<Chained>;}
    member this.CalculateProbabilities() =
        let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) this.joined));
        for sequence in this.joined do
            sequence.probability <- (float) sequence.value.Length / overallAmount;


let getChain(collectedSubchains: List<List<string>>) =
    let mutable chain  = new Collections.Generic.Dictionary<string, Link>();
    for subchain in collectedSubchains do
        let sequitur = [for i = 1 to subchain.Length - 1 do subchain[i]];
        if (chain.Keys.Contains(subchain[0])) then
            if List.exists(fun x -> List.forall2(fun elem1 elem2 -> String.Equals(elem1, elem2)) x.value sequitur) chain[subchain[0]].joined then
                (List.find(fun x -> List.forall2(fun elem1 elem2 -> String.Equals(elem1, elem2)) x.value sequitur) chain[subchain[0]].joined).amount <- (List.find(fun x -> List.forall2(fun elem1 elem2 -> String.Equals(elem1, elem2)) x.value sequitur) chain[subchain[0]].joined).amount + 1;
            else
                chain[subchain[0]].joined <- {value = sequitur; amount = 1; probability = 0} :: chain[subchain[0]].joined;
        else
            chain[subchain[0]] <- {joined = [{value = sequitur; amount = 1; probability = 0}]};
    for link in chain do
        link.Value.CalculateProbabilities();
    chain;

for link in getChain(CollectSubchains(sentences, 2)) do
    printfn "%s :" link.Key
    for value in link.Value.joined do
        printf "\t %s - %.2f\n" (String.concat " " value.value) value.probability;
    printfn "";
