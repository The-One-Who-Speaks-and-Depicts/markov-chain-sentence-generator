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
    output;

let appendItem seqToAppendTo itemToAppend =
    [ yield! seqToAppendTo
      yield itemToAppend ]

let GetNGram (tokenPos: int, n: int, sentence: List<string>) =
    let mutable output : List<string> = [];
    for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then output <- appendItem output sentence[i] else output <- appendItem output "#END";
    output;


let CollectSubchains (sentences: List<List<string>>, n: int) =
    let mutable output : List<List<string>> = [];
    for sentence in sentences do
        for i = 0 to sentence.Length - 1 do
            output <- appendItem output (GetNGram(i, n, sentence));
    output;

type Chained =
    {value: List<string>;
    mutable amount: int;
    mutable probability: float;}

    member this.IncrementAmount() =
        this.amount <- this.amount + 1;

    member this.CalculateProbability(overallAmount: float) =
        this.probability <- (float) this.value.Length / overallAmount;



type Link =
    {mutable joined: List<Chained>;}

    member this.CalculateProbabilities() =
        let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) this.joined));
        for sequence in this.joined do
            sequence.CalculateProbability(overallAmount);

    member this.ExistingTail (tail : List<string>) = List.tryFind(fun x -> List.forall2(fun elem1 elem2 -> String.Equals(elem1, elem2)) x.value tail) this.joined;

    member this.ReturnRandomWord() =
        let random = Random().NextDouble();
        let tails = List.map (fun x -> {value = x.value; probability = abs x.probability - random; amount = x.amount}) this.joined;
        tails.Where(fun x -> x.probability.Equals(tails.Select(fun x -> x.probability).Max())).First().value;



let getChain(collectedSubchains: List<List<string>>) =
    let mutable chain  = new Collections.Generic.Dictionary<string, Link>();
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

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

let generate(chain : Collections.Generic.Dictionary<string, Link>) =
    let mutable finalSentence = ["#START"];
    while not (String.Equals (last finalSentence, "#END")) do
        finalSentence <- finalSentence @ (chain[last finalSentence].ReturnRandomWord());
    finalSentence;

printfn "%s" (SentenceOutput(generate(getChain(CollectSubchains(sentences, 2)))));
