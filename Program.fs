// TODO: Markov Chain Sentence Generator

open System;
open System.Linq;
open System.Collections.Generic;
open System.IO;
open FSharp.Data;
open ShellProgressBar;

let dir = "F:\\DB\\mcsg\\russian-troll-tweets";

let AppendItem seqToAppendTo itemToAppend =
    [ yield! seqToAppendTo
      yield itemToAppend ]

let rec Last = function
    | hd :: [] -> hd
    | hd :: tl -> Last tl
    | _ -> failwith "Empty list."

let tokenize (sentence: string) =
    "#START" :: List.ofSeq(sentence.Split(" "))



let loadFiles(dir: string) =
    let totalTicks = DirectoryInfo(dir).GetFiles().Length;
    let options = new ProgressBarOptions ();
    options.ProgressCharacter <- '*';
    options.ProgressBarOnBottom <- true;
    let bar = new ProgressBar(totalTicks, " files loaded", options);
    let barProgress = using (bar)
    (
        [for file in DirectoryInfo(dir).GetFiles() do
            bar.Tick();
            if file.FullName.EndsWith(".csv") then
                yield! (List.ofSeq ((CsvFile.Load(file.FullName).Rows).Select(fun x -> tokenize x[2])))
        ]
    );


let SentenceOutput(sentence: list<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output;

let GetNGram (tokenPos: int, n: int, sentence: list<string>) =
    let mutable output : list<string> = [];
    for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then output <- AppendItem output sentence[i] else output <- AppendItem output "#END";
    output;


let CollectSubchains (sentences: list<list<string>>) =
    let totalTicks = sentences.Length;
    let options = new ProgressBarOptions ();
    options.ProgressCharacter <- '*';
    options.ProgressBarOnBottom <- true;
    let bar = new ProgressBar(totalTicks, " sentences preprocessed", options);
    let barProgress = using (bar)
    (
        [for sentence in sentences do
            bar.Tick();
            yield! [for i = 0 to sentence.Length do
                    yield GetNGram(i, 2, sentence)
            ]
        ]
    );

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
        let random = Random().Next(0, this.joined.Length);
        this.joined[random].value;

let GetFullChain(collectedSubchains: list<list<string>>) =
    let totalTicks = collectedSubchains.Length;
    let options = new ProgressBarOptions ();
    options.ProgressCharacter <- '*';
    options.ProgressBarOnBottom <- true;
    let bar = new ProgressBar(totalTicks, " subchains preprocessed", options);
    let flow = using (bar)
    (
        [for subchain in collectedSubchains do
            bar.Tick();
            yield KeyValuePair.Create(subchain.Head, subchain.Tail);
        ];
    )

type Chain =
    {mutable links: list<KeyValuePair<string, Link>>;}

    member this.CalculateProbabilitiesForChain() =
        for link in this.links do
            link.Value.CalculateProbabilities();

let GetChain (fullChain: list<KeyValuePair<string, list<string>>>) =
    let groupedPairs = fullChain.GroupBy(fun x -> x.Key);
    let totalTicks = groupedPairs.Count();
    let options = new ProgressBarOptions ();
    options.ProgressCharacter <- '*';
    options.ProgressBarOnBottom <- true;
    let bar = new ProgressBar(totalTicks, " keys preprocessed", options);
    let flow = using (bar)
    (
        let chain = {links = [for group in groupedPairs do
                                bar.Tick();
                                let subgroup = group.GroupBy(fun x -> x.Value);
                                yield KeyValuePair.Create(group.Key, {joined = [for infragroup in subgroup do
                                                                                    yield {value = infragroup.Key; amount = infragroup.Count(); probability = 0}
                                ]})
        ]}
        chain.CalculateProbabilitiesForChain();
        chain;

    )


let Generate(chain : Chain) =
    let mutable finalSentence = ["#START"];
    while ((not (String.Equals (Last finalSentence, "#END"))) && ([for i in finalSentence do i.Length].Sum() < 286)) do
        finalSentence <- finalSentence @ (chain.links.Where(fun x -> x.Key = Last finalSentence).First().Value.ReturnRandomWord());
    finalSentence;

printfn "%s" (dir |> loadFiles |> CollectSubchains |> GetFullChain |> GetChain |> Generate |> SentenceOutput);
