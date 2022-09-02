// TODO: Markov Chain Sentence Generator
// TODO: include unit tests?
// TODO: add detoxifier/toxicity scaler?
// TODO: separate tokenizer

open System
open System.Linq
open System.Collections.Generic
open System.IO
open FSharp.Data
open ShellProgressBar

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."


let stringSplit (str : string) (sep:string) = List.ofSeq (str.Split(sep));


// TODO: alternative tokenizer and interfacing tokenizer
// TODO: href/mention deletion
let tokenize (sentence: string) =
    "#START" :: List.ofSeq(stringSplit sentence " ")


let initializeBar ticks character inscription onBottom =
    let options = new ProgressBarOptions();
    options.ProgressCharacter <- character;
    options.ProgressBarOnBottom <- onBottom;
    new ProgressBar(ticks, inscription, options)


let getFilesFromDir dir  = DirectoryInfo(dir).GetFiles()

let loadFiles (dir: string) =
    let files = List.ofSeq (getFilesFromDir dir) |> List.filter (fun x -> last (stringSplit x.FullName ".") = "csv");
    let bar = initializeBar (files.Length) '*' "files loaded" true;
    let progression = using (bar)
    (
        files |> List.ofSeq |> List.map(fun x -> bar.Tick(); (CsvFile.Load(x.FullName).Rows)) |> List.map(fun x -> List.ofSeq (x.Select(fun x -> tokenize x[2]))) |> List.concat
    )




// TODO: output of parentheses
let rec sentenceOutput (output : string) (sentence : list<string>) =
    match sentence.Length with
        | 0 -> output
        | _ -> match sentence.Head with
                    | "," | ";" | ":" | "!" | "." | "?" -> [sentence.Head; sentenceOutput output sentence.Tail] |> String.concat ""
                    | "#START" | "#END" -> sentenceOutput output sentence.Tail
                    | _ -> [sentence.Head; sentenceOutput output sentence.Tail] |> String.concat " "

let getNGram(tokenPos : int)  (n : int) (sentence : list<string>) =
    [tokenPos .. (tokenPos + n - 1)] |> List.map (fun x ->
                                                                match x with
                                                                    | i when i < sentence.Length -> sentence[i]
                                                                    | _ -> "#END")


let collectSubchains n (sentences : list<list<string>>) =
    let bar = initializeBar sentences.Length '*' " sentences preprocessed." true;
    let barProgress = using (bar)
    (
        sentences |> List.map (fun x -> bar.Tick(); [0 .. x.Length] |> List.map(fun y -> getNGram y n x)) |> List.concat
    );

type Chained =
    {value: list<string>;
    amount: int;
    probability: float;}

let calculateProbabilityForChained chained overallAmount =
    {value = chained.value; amount = chained.amount; probability = (float) chained.value.Length / overallAmount};

type Link =
    {joined: list<Chained>;}

    // TODO 2 : not so random with (span of 0... prob1 ... prob2 (...) probN (...) 1)
    member this.returnRandomWord(seed: Option<int>) =
        match seed with
            | Some seed -> this.joined[Random(seed).Next(0, this.joined.Length)].value
            | None -> this.joined[Random().Next(0, this.joined.Length)].value

let calculateProbabilitiesForLink link =
    let overallAmount  = (float) (List.reduce(fun x y -> x + y) (List.map (fun x -> x.value.Length) link.joined));
    {joined = link.joined |> List.map (fun x -> calculateProbabilityForChained x overallAmount)}


let getFullChain(collectedSubchains: list<list<string>>) =
    let bar = initializeBar collectedSubchains.Length '*' " subchains preprocessed." true;
    let flow = using (bar)
    (
        collectedSubchains |> List.map(fun x -> bar.Tick(); KeyValuePair.Create(x.Head, x.Tail))
    )

type Chain =
    {links: list<KeyValuePair<string, Link>>;}

let calculateProbabilitiesForChain chain =
    {links = chain.links |> List.map (fun x -> KeyValuePair.Create(x.Key, calculateProbabilitiesForLink x.Value))}

let getChain (fullChain: list<KeyValuePair<string, list<string>>>) =
    let groupedPairs = fullChain.GroupBy(fun x -> x.Key);
    let bar = initializeBar (groupedPairs.Count()) '*' " keys preprocessed" true;
    let flow = using (bar)
    (
        calculateProbabilitiesForChain {links = groupedPairs |> List.ofSeq |> List.map(fun group -> bar.Tick();KeyValuePair.Create(group.Key, {joined = group.GroupBy(fun y -> y.Value) |> List.ofSeq |> List.map (fun x -> {value = x.Key; amount = x.Count(); probability = 0})}))}
    )

let rec generate finalSentence (seed: Option<int>) (chain : Chain)  =
    match (last finalSentence) with
        | "#END" -> finalSentence
        | _ ->
                match ([for i in finalSentence do i.Length].Sum()) with
                        |  j when j > 286 -> finalSentence
                        | _ -> generate (finalSentence @ (chain.links.Where(fun x -> x.Key = last finalSentence).First().Value.returnRandomWord(seed))) seed chain;

[<EntryPoint>]
let main argv =
    let dir = match argv.Length with
                    | i when i > 0 ->
                                            match Directory.Exists(argv[0]) with
                                                | true -> argv[0]
                                                | false -> "Empty"
                    | _ -> failwith "No data provided."
    let nGrams = match argv.Length with
                        | i when i > 1 ->
                                                match Int32.TryParse argv[1] with
                                                | (true, int) -> Some(int).Value
                                                | _ -> 2
                        | _ -> 2
    printfn "Using %d-grams." nGrams;
    let seed = match argv.Length with
                        | i when i > 2 ->
                                                match Int32.TryParse argv[2] with
                                                | (true, int) -> printfn "Set seed to %d." int; Some(int)
                                                | _ -> None
                        | _ -> None
    printfn "%s" (dir|> loadFiles |> collectSubchains nGrams |> getFullChain |> getChain |> generate ["#START"] seed |> sentenceOutput "");
    0
