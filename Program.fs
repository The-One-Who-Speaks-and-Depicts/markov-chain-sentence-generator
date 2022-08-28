// TODO: Markov Chain Sentence Generator
// TODO: include unit tests?
// TODO: add detoxifier/toxicity scaler?
// TODO: separate tokenizer

open System
open System.Linq
open System.Collections.Generic
open System.IO
open System.Text
open FSharp.Data
open ShellProgressBar
open System.Net.Http
open System.Text.Json

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."


// TODO: alternative tokenizer and interfacing tokenizer
// TODO: href/mention deletion
let tokenize (sentence: string) =
    "#START" :: List.ofSeq(sentence.Split(" "))


let initializeBar ticks character inscription onBottom =
    let options = new ProgressBarOptions();
    options.ProgressCharacter <- character;
    options.ProgressBarOnBottom <- onBottom;
    new ProgressBar(ticks, inscription, options)


let loadFiles(dir: string) =
    let bar = initializeBar (DirectoryInfo(dir).GetFiles().Length) '*' "files loaded" true;
    let barProgress = using (bar)
    (
        [for file in DirectoryInfo(dir).GetFiles() do
            bar.Tick();
            if file.FullName.EndsWith(".csv") then
                yield! (List.ofSeq ((CsvFile.Load(file.FullName).Rows).Select(fun x -> tokenize x[2])))
        ]
    );

// TODO: output of parentheses
let rec sentenceOutput (output : string) (sentence : list<string>) =
    if sentence.Length = 0 then output else
        match sentence.Head with
        | "," | ";" | ":" | "!" | "." | "?" -> ((new StringBuilder()).Append(sentence.Head).Append(sentenceOutput output sentence.Tail)).ToString()
        | "#START" | "#END" -> sentenceOutput output sentence.Tail
        | _ -> ((new StringBuilder()).Append(sentence.Head).Append(" ").Append(sentenceOutput output sentence.Tail)).ToString()

let getNGram(tokenPos : int, n : int, sentence : list<string>) =
    [for i = tokenPos to tokenPos + n - 1 do
        if i < sentence.Length then
            yield sentence[i]
        else
            yield "#END"
    ]

let collectSubchains n (sentences : list<list<string>>) =
    let bar = initializeBar sentences.Length '*' " sentences preprocessed." true;
    let barProgress = using (bar)
    (
        [for sentence in sentences do
            bar.Tick();
            yield! [for i = 0 to sentence.Length do
                    yield getNGram(i, n, sentence)
            ]
        ]
    );

type Chained =
    {value: list<string>;
    mutable amount: int;
    mutable probability: float;}

    member this.calculateProbability(overallAmount: float) =
        this.probability <- (float) this.value.Length / overallAmount;

type Link =
    {mutable joined: list<Chained>;}

    member this.calculateProbabilities() =
        let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) this.joined));
        for sequence in this.joined do
            sequence.calculateProbability(overallAmount);

    // TODO 2 : not so random with (span of 0... prob1 ... prob2 (...) probN (...) 1)
    member this.returnRandomWord(seed: Option<int>) =
        if (seed.IsSome) then
            this.joined[Random(seed.Value).Next(0, this.joined.Length)].value
        else
            this.joined[Random().Next(0, this.joined.Length)].value

let getFullChain(collectedSubchains: list<list<string>>) =
    let bar = initializeBar collectedSubchains.Length '*' " subchains preprocessed." true;
    let flow = using (bar)
    (
        [for subchain in collectedSubchains do
            bar.Tick();
            yield KeyValuePair.Create(subchain.Head, subchain.Tail);
        ];
    )

type Chain =
    {mutable links: list<KeyValuePair<string, Link>>;}

    member this.calculateProbabilitiesForChain() =
        for link in this.links do
            link.Value.calculateProbabilities();

let getChain (fullChain: list<KeyValuePair<string, list<string>>>) =
    let groupedPairs = fullChain.GroupBy(fun x -> x.Key);
    let bar = initializeBar (groupedPairs.Count()) '*' " keys preprocessed" true;
    let flow = using (bar)
    (
        let chain = {links = [for group in groupedPairs do
                                bar.Tick();
                                let subgroup = group.GroupBy(fun x -> x.Value);
                                yield KeyValuePair.Create(group.Key, {joined = [for infragroup in subgroup do
                                                                                    yield {value = infragroup.Key; amount = infragroup.Count(); probability = 0}
                                ]})
        ]}
        chain.calculateProbabilitiesForChain();
        chain;
    )


let rec generate finalSentence (seed: Option<int>) (chain : Chain)  =
    if (String.Equals (last finalSentence, "#END")) || ([for i in finalSentence do i.Length].Sum() > 286) then
        finalSentence
    else generate (finalSentence @ (chain.links.Where(fun x -> x.Key = last finalSentence).First().Value.returnRandomWord(seed))) seed chain;

[<EntryPoint>]
let main argv =
    if (argv.Length > 0 && Directory.Exists(argv[0])) then
        let nGrams = if argv.Length > 1 then
                        match Int32.TryParse argv[1] with
                        | (true, int) -> Some(int).Value
                        | _ -> 2
                        else
                            2
        printfn "Using %d-grams." nGrams;
        let seed = if argv.Length > 2 then
                        match Int32.TryParse argv[2] with
                        | (true, int) -> Some(int)
                        | _ -> None
                        else
                            None
        if seed.IsSome then printfn "Set seed to %d." seed.Value;
        printfn "%s" (argv[0] |> loadFiles |> collectSubchains nGrams |> getFullChain |> getChain |> generate ["#START"] seed |> sentenceOutput "");
    else
        printfn "%s" "No data provided."
    0
