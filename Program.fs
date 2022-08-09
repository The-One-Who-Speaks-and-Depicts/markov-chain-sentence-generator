// TODO: Markov Chain Sentence Generator
// TODO: include unit tests?

open System;
open System.Linq;
open System.Collections.Generic;
open System.IO;
open FSharp.Data;
open ShellProgressBar;

let rec Last = function
    | hd :: [] -> hd
    | hd :: tl -> Last tl
    | _ -> failwith "Empty list."


// TODO: separate tokenizer
let tokenize (sentence: string) =
    "#START" :: List.ofSeq(sentence.Split(" "))


let InitializeBar ticks character inscription onBottom =
    let options = new ProgressBarOptions();
    options.ProgressCharacter <- character;
    options.ProgressBarOnBottom <- onBottom;
    new ProgressBar(ticks, inscription, options)


let loadFiles(dir: string) =
    let bar = InitializeBar (DirectoryInfo(dir).GetFiles().Length) '*' "files loaded" true;
    let barProgress = using (bar)
    (
        [for file in DirectoryInfo(dir).GetFiles() do
            bar.Tick();
            if file.FullName.EndsWith(".csv") then
                yield! (List.ofSeq ((CsvFile.Load(file.FullName).Rows).Select(fun x -> tokenize x[2])))
        ]
    );

// TODO: output of parentheses
let SentenceOutput(sentence: list<string>) =
    let mutable output = "";
    for token in sentence do
        match token with
        | "," | ";" | ":" | "!" | "." | "?"  -> output <- String.concat "" [output; token]
        | "#START" | "#END" -> output <- output
        | _ -> output <- if String.Equals("", output) then token else String.concat " " [output; token]
    output;

let GetNGram(tokenPos : int, n : int, sentence : list<string>) =
    [for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then
            yield sentence[i]
        else
            yield "#END"
    ]

let CollectSubchains n sents =
    let sentences = List.ofSeq [for s in sents do List.ofSeq s];
    let bar = InitializeBar sentences.Length '*' " sentences preprocessed." true;
    let barProgress = using (bar)
    (
        [for sentence in sentences do
            bar.Tick();
            yield! [for i = 0 to sentence.Length do
                    yield GetNGram(i, n, sentence)
            ]
        ]
    );

type Chained =
    {value: list<string>;
    mutable amount: int;
    mutable probability: float;}

    member this.CalculateProbability(overallAmount: float) =
        this.probability <- (float) this.value.Length / overallAmount;

type Link =
    {mutable joined: list<Chained>;}

    member this.CalculateProbabilities() =
        let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) this.joined));
        for sequence in this.joined do
            sequence.CalculateProbability(overallAmount);

    // TODO 1 : set seed with https://www.tutorialsteacher.com/articles/generate-random-numbers-in-csharp
    // TODO 2 : not so random with (span of 0... prob1 ... prob2 (...) probN (...) 1)
    member this.ReturnRandomWord() =
        let random = Random().Next(0, this.joined.Length);
        this.joined[random].value;

let GetFullChain(collectedSubchains: list<list<string>>) =
    let bar = InitializeBar collectedSubchains.Length '*' " subchains preprocessed." true;
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
    let bar = InitializeBar (groupedPairs.Count()) '*' " keys preprocessed" true;
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


let rec Generate finalSentence (chain : Chain) =
    if (String.Equals (Last finalSentence, "#END")) || ([for i in finalSentence do i.Length].Sum() < 286) then finalSentence
    else Generate (finalSentence @ (chain.links.Where(fun x -> x.Key = Last finalSentence).First().Value.ReturnRandomWord())) chain;

[<EntryPoint>]
let main argv =
    let NGrams = match Int32.TryParse argv[1] with
                    | (true, int) -> Some(int)
                    | _ -> None
    printfn "%s" (argv[0] |> loadFiles |> CollectSubchains (if NGrams.IsSome then NGrams.Value else 2) |> GetFullChain |> GetChain |> Generate ["#START"] |> SentenceOutput);
    0
