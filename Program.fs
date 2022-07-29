// For more information see https://aka.ms/fsharp-console-apps

let sentences = [ ["#START"; "I"; "swim"; "."; "#END"]; ["#START"; "I"; "go"; "swimming"; "."; "#END"] ] ;

let sentenceOutput(sentence: List<string>) = String.concat " " sentence


for sentence in sentences do
    printfn "%s \n" (sentenceOutput(sentence))
