type Word = Word of string
let unwrapWord (Word w) = w
type Tag = Tag of string
let unwrapTag (Tag t) = t
type Feature = Feature of string

type Prediction =
    { Word:Word
      ActualTag:Tag
      PredictedTag:Tag
      Features:Feature list }

type Model = 
    { Tags : Tag list
      Weights : Map<Feature, Map<Tag, float>> }

//=======================================================//

// ADD TEST TRAINING DATA HERE

let trainingSet = [ [ (Word "my", Tag "MY"); (Word "name", Tag "NAME"); (Word "is", Tag "IS"); (Word "rich", Tag "RICH") ]
                    [ (Word "what", Tag "WHAT"); (Word "is", Tag "IS"); (Word "your", Tag "YOUR"); (Word "name", Tag "NAME") ] ]

//=======================================================//

let getTaggingContext (taggedSentence:(Word * Tag) list)                                                       // getTrainingContext
    : (Word * Tag) list = 
        let capOne = (Word "_", Tag "_")
        let capTwo = (Word "__", Tag "__")
        let taggingContext = 
            [ capTwo; capOne ] @ taggedSentence @ [ capOne; capTwo ]
        taggingContext

//=======================================================//




let getFeatures (context:(Word * Tag) list) =

    let getFirstCharacter (word:Word) =  
        string (unwrapWord word).[0]    
    
    let getSuffix (word:Word) =   
        let unwrappedWord = unwrapWord word
        let suffix (w:string) =
            let startOfSuffix = (String.length w) - 3
            let endOfWord = (String.length w) - 1
            w.[startOfSuffix .. endOfWord]
        match (String.length unwrappedWord) with
            | l when l <= 3 -> unwrappedWord
            | l when l >= 4 -> unwrappedWord |> suffix
            | _ -> "Something went terribly wrong here..."

    let featuresList = 
        [ Feature ("bias");
          Feature ("word_" + unwrapWord (fst context.[2]));
          Feature ("word_suffix_" + ((fst context.[2]) |> getSuffix));
          Feature ("word_first_letter_" + ((fst context.[2]) |> getFirstCharacter));
          Feature ("previous_word_" + unwrapWord (fst context.[1]));
          Feature ("previous_word_suffix_" + ((fst context.[1]) |> getSuffix));
          Feature ("previous_word_tag_" + unwrapTag (snd context.[1]));
          Feature ("second_previous_word_" + unwrapWord (fst context.[0]));
          Feature ("second_previous_word_tag_" + unwrapTag (snd context.[0]));
          Feature ("following_word_" + unwrapWord (fst context.[3]));
          Feature ("following_word_suffix_" + ((fst context.[3]) |> getSuffix));
          Feature ("second_following_word_" + unwrapWord (fst context.[4]));
          Feature ("word_tag_plus_second_previous_word_tag_" + unwrapTag (snd context.[2]) + "_" + unwrapTag (snd context.[0])); 
          Feature ("previous_word_tag_plus_word_" + (unwrapTag (snd context.[1]) + "_" + unwrapWord (fst context.[2]))) ]
    (fst context.[2]), (snd context.[2]), featuresList

//=======================================================//





let makePredictionScoreList (weights:Map<Feature, Map<Tag, float>>) (feature:Feature) =                                                                   // predictTagOnFeatures helper
    let featureWeights = weights.[feature]
                            |> Map.toList
                            |> List.map (fun tagWeight -> (fst tagWeight, snd tagWeight)) 
    featureWeights


let predictTagOnFeatures (weights:Map<Feature, Map<Tag, float>>) (wordFeatures:(Word * Tag * Feature list)) =   // predictTagOnFeatures
    
    let word, tag, features = wordFeatures

    // Hacky solution to ensure something returns. The "::" tag should never be matched correctly with training data and will be reduced into oblivion.
    let testWeights = features
                          |> List.fold (fun state feature -> 
                                            match (weights.TryFind feature) with
                                                | Some value -> state
                                                | None -> state |> Map.add feature (Map.empty.Add(Tag "::", 0.0))
                                       ) weights

    let prediction = features
                      |> List.map (makePredictionScoreList testWeights)
                      |> List.reduce (fun listOne listTwo -> listOne @ listTwo)
                      |> List.sortByDescending (fun (_,score) -> score)
                      |> List.maxBy (fun (_,score) -> score)

    { Word = word; ActualTag = tag; PredictedTag = fst prediction; Features = features }

//=======================================================//
// From https://www.rosettacode.org/wiki/Knuth_shuffle#F.23

open System

let knuthShuffle (arr:array<'a>) =
    let Swap i j =
        let item = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- item
    let rnd = new Random()
    let length = arr.Length
    [0..(length - 2)]
        |> Seq.iter (fun i -> Swap i (rnd.Next(i, length)))
    arr

//=======================================================//

let updateWeights (weights: Map<Feature, Map<Tag, float>>) (prediction:Prediction) : Map<Feature, Map<Tag, float>> = 

    let update (state:Map<Feature, Map<Tag, float>>) (tag:Tag) (changeValue:float) (feature: Feature) =
        match (state.TryFind feature) with
            | Some tagWeight -> 
                    let newTagWeight = match (tagWeight.TryFind tag) with
                                        | Some w ->
                                            tagWeight |> Map.add tag (w + changeValue)                                            
                                        | None -> tagWeight |> Map.add tag changeValue
                    state |> Map.add feature newTagWeight
            | None ->
                    let tagMap = Map.empty.Add(tag, changeValue)
                    state |> Map.add feature tagMap                   
        

    match (prediction.PredictedTag = prediction.ActualTag) with
        | true -> weights
        | false -> 
            prediction.Features |> List.fold (fun weightsToUpdate feat -> 
                                                let updatedActualWeights = feat |> update weightsToUpdate prediction.ActualTag 1.0 
                                                let updatedPredictedWeights = feat |> update updatedActualWeights prediction.PredictedTag -1.0
                                                updatedPredictedWeights
                                             ) weights 

//=======================================================//

let trainingIteration (weights: Map<Feature, Map<Tag, float>>) (sentences:(Word * Tag) list list) : Map<Feature, Map<Tag, float>> = 
    let iterationWeights = sentences
                            |> List.map getTaggingContext
                            |> List.map (fun sentence -> sentence |> List.windowed 5)
                            |> List.fold (fun intermediateWeights windowedSentence ->
                                            windowedSentence |> List.fold (fun trainingWeights trainingWindow ->
                                                                              trainingWindow
                                                                                |> getFeatures
                                                                                |> predictTagOnFeatures trainingWeights
                                                                                |> updateWeights trainingWeights
                                                                          ) intermediateWeights
                                            ) weights
    iterationWeights


//=======================================================//

let trainForIterations (iterations:int) (sentences:((Word * Tag) list) list) =
    //-- Create shuffled sentence list to use with List.fold.
    let rec createSentenceIterations iters sentList sents = 
        if iters = 0 then
            List.rev sentList
        else 
            let shuffledSentence = sents |> List.toArray |> knuthShuffle |> Array.toList
            let newSentList = (shuffledSentence :: sentList)
            (sents |> createSentenceIterations (iters - 1) newSentList)

    let shuffledSentencesForIterations = sentences |> createSentenceIterations (iterations) []
    //---------------------------------------------------

    let finalWeights = shuffledSentencesForIterations
                        |> List.fold trainingIteration Map.empty                                                           
    finalWeights

//=======================================================//





let getTags (sentences:((Word * Tag) list) list) =
    let reducer listOne listTwo = listOne @ listTwo

    let tags = 
        sentences
            |> List.reduce reducer
            |> List.unzip

    (snd tags) |> List.distinct



//=======================================================//



    
        