// TODO: Markov Chain Sentence Generator

open System;
open System.Linq;
open System.Collections.Generic;


let sentences = [ ["#START"; "I"; "swim"; "."]; ["#START"; "I"; "go"; "swimming"; "."] ] ;

let SentenceOutput(sentence: list<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output;

let AppendItem seqToAppendTo itemToAppend =
    [ yield! seqToAppendTo
      yield itemToAppend ]

let GetNGram (tokenPos: int, n: int, sentence: list<string>) =
    let mutable output : list<string> = [];
    for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then output <- AppendItem output sentence[i] else output <- AppendItem output "#END";
    output;


let CollectSubchains (sentences: list<list<string>>) =
    let mutable output : list<list<string>> = [];
    for sentence in sentences do
        for i = 0 to sentence.Length - 1 do
            output <- AppendItem output (GetNGram(i, 2, sentence));
    output;

type Chained =
    {value: list<string>;
    mutable amount: int;
    mutable probability: float;}

    member this.IncrementAmount() =
        this.amount <- this.amount + 1;

    member this.CalculateProbability(overallAmount: float) =
        this.probability <- (float) this.value.Length / overallAmount;



type Link =
    {mutable joined: list<Chained>;}

    member this.CalculateProbabilities() =
        let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) this.joined));
        for sequence in this.joined do
            sequence.CalculateProbability(overallAmount);

    member this.ExistingTail (tail : list<string>) = List.tryFind(fun x -> List.forall2(fun elem1 elem2 -> String.Equals(elem1, elem2)) x.value tail) this.joined;

    member this.ReturnRandomWord() =
        let random = Random().NextDouble();
        let tails = List.map (fun x -> {value = x.value; probability = abs x.probability - random; amount = x.amount}) this.joined;
        tails.Where(fun x -> x.probability.Equals(tails.Select(fun x -> x.probability).Max())).First().value;



let GetChain(collectedSubchains: list<list<string>>) =
    let mutable chain  = new Dictionary<string, Link>();
    for subchain in collectedSubchains do
        if (List.tryFind(fun x -> String.Equals(x, subchain.Head)) (List.ofSeq(chain.Keys))).IsSome then
            let listWithTail = chain[subchain.Head].ExistingTail(subchain.Tail);
            if listWithTail.IsSome then
                listWithTail.Value.IncrementAmount();
            else
                chain[subchain.Head].joined <- {value = subchain.Tail; amount = 1; probability = 0} :: chain[subchain.Head].joined;
        else
            chain[subchain.Head] <- {joined = [{value = subchain.Tail; amount = 1; probability = 0}]};
    for link in chain do
        link.Value.CalculateProbabilities();
    chain;

let rec Last = function
    | hd :: [] -> hd
    | hd :: tl -> Last tl
    | _ -> failwith "Empty list."

let Generate(chain : Dictionary<string, Link>) =
    let mutable finalSentence = ["#START"];
    while not (String.Equals (Last finalSentence, "#END")) do
        finalSentence <- finalSentence @ (chain[Last finalSentence].ReturnRandomWord());
    finalSentence;

printfn "%s" (sentences |> CollectSubchains |> GetChain |> Generate |> SentenceOutput);
