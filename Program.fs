// TODO: Markov Chain Sentence Generator

let sentences = [ ["#START"; "I"; "swim"; "."; "#END"]; ["#START"; "I"; "go"; "swimming"; "."; "#END"] ] ;

let sentenceOutput(sentence: List<string>) = String.concat " " sentence


for sentence in sentences do
    printfn "%s \n" (sentenceOutput(sentence))
