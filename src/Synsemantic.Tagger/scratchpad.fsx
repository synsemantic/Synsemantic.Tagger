﻿type Word = Word of string
let unwrapWord (Word w) = w
type Tag = Tag of string
let unwrapTag (Tag t) = t
type Feature = Feature of string

type WeightTracker = 
    { Weights:Map<Feature, Map<Tag, float>>
      TrainingAccumulator:Map<(Feature * Tag), float>
      InstanceLastSeen:Map<(Feature * Tag), int>
      CurrentIteration:int
      CurrentInstance:int }

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

let getTaggingContext (taggedSentence:(Word * Tag) list) 
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

let makePredictionScoreList (weights:Map<Feature, Map<Tag, float>>) (feature:Feature) =
    let featureWeights = weights.[feature]
                            |> Map.toList
                            |> List.map (fun tagWeight -> (fst tagWeight, snd tagWeight)) 
    featureWeights


let predictTagOnFeatures (weights:WeightTracker) (wordFeatures:(Word * Tag * Feature list)) =
    
    let word, tag, features = wordFeatures

    // Hacky solution to ensure something returns. The "::" tag should never be matched correctly with training data and will be reduced into oblivion.
    let testWeights = features
                          |> List.fold (fun state feature -> 
                                            match (weights.Weights.TryFind feature) with
                                                | Some value -> state
                                                | None -> state |> Map.add feature (Map.empty.Add(Tag "::", 0.0))
                                       ) weights.Weights

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

let updateWeightTracker (featureTagKey:(Feature * Tag)) (tagMap:Map<Tag, float>) (changeValue:float) (weights:WeightTracker) : WeightTracker = 
    let instanceLastSeen = 
        match (weights.InstanceLastSeen.TryFind featureTagKey) with
            | Some value -> value
            | None -> 0
    
    let catchUpWeight = (float (weights.CurrentInstance - instanceLastSeen)) * ((tagMap.[snd featureTagKey]) + changeValue)

    let newAccumulatorWeight = 
        match (weights.TrainingAccumulator.TryFind featureTagKey) with
            | Some currentAccumulatorValue  -> currentAccumulatorValue + catchUpWeight
            | None -> catchUpWeight

    { weights with 
        Weights = weights.Weights |> Map.add (fst featureTagKey) tagMap
        TrainingAccumulator = weights.TrainingAccumulator |> Map.add featureTagKey newAccumulatorWeight
        InstanceLastSeen = weights.InstanceLastSeen |> Map.add featureTagKey weights.CurrentInstance }

//=======================================================//

let updateWeights (weights:WeightTracker) (prediction:Prediction) : WeightTracker = 
    let update (state:WeightTracker) (tag:Tag) (changeValue:float) (feature: Feature) =
        match (state.Weights.TryFind feature) with
            | Some tagWeightMap -> 
                    let newTagWeightMap = match (tagWeightMap.TryFind tag) with
                                            | Some w ->
                                                tagWeightMap |> Map.add tag (w + changeValue)                                            
                                            | None -> tagWeightMap |> Map.add tag changeValue
                    state |> updateWeightTracker (feature, tag) newTagWeightMap changeValue
            | None ->
                    let newTagWeightMap = Map.empty.Add(tag, changeValue)
                    state |> updateWeightTracker (feature, tag) newTagWeightMap changeValue                 
        

    let updatedWeightTracker = 
            match (prediction.PredictedTag = prediction.ActualTag) with
            | true -> weights
            | false -> 
                prediction.Features |> List.fold (fun weightsToUpdate feat -> 
                                                    let updatedActualWeights = feat |> update weightsToUpdate prediction.ActualTag 1.0 
                                                    let updatedPredictedWeights = feat |> update updatedActualWeights prediction.PredictedTag -1.0
                                                    updatedPredictedWeights
                                                 ) weights 

    { updatedWeightTracker with 
        CurrentInstance = updatedWeightTracker.CurrentInstance + 1 }

//=======================================================//

let trainingIteration (weights:WeightTracker) (sentences:(Word * Tag) list list) : WeightTracker = 
    printfn "Starting new iteration, current iteration: %d" weights.CurrentIteration
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
    { iterationWeights with
        CurrentIteration = iterationWeights.CurrentIteration + 1 }

//=======================================================//

let averageWeights (weightTracker:WeightTracker) : Map<Feature, Map<Tag, float>> = 
    let currentWeights = weightTracker.Weights |> Map.toList

    let newWeights = currentWeights |> List.map (fun (feature, tagMap) -> 
                                           let newTagMap = tagMap 
                                                               |> Map.toList  
                                                               |> List.map (fun (tag, weight) ->
                                                                               let featureTag = (feature, tag)
                                                                               let catchUpTotal = (float (weightTracker.CurrentInstance - weightTracker.InstanceLastSeen.[featureTag])) * weight
                                                                               let total = (weightTracker.TrainingAccumulator.[featureTag]) + catchUpTotal
                                                                               let averagedWeight = System.Math.Round(total / float(weightTracker.CurrentInstance), 3)
                                                                               (tag, averagedWeight) )
                                                               |> Map.ofList 
                                           (feature, newTagMap) )

    newWeights |> Map.ofList
                                
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

    shuffledSentencesForIterations
        |> List.fold trainingIteration { Weights = Map.empty; TrainingAccumulator = Map.empty; InstanceLastSeen = Map.empty; CurrentIteration = 1; CurrentInstance = 1 }
        |> averageWeights

//=======================================================//

let getTags (sentences:((Word * Tag) list) list) =
    let reducer listOne listTwo = listOne @ listTwo

    let tags = 
        sentences
            |> List.reduce reducer
            |> List.unzip

    (snd tags) |> List.distinct

//=======================================================//

let train (iterations:int) (sentences:((Word * Tag) list) list) : Model =

    let model = 
        { Tags = sentences |> getTags
          Weights = sentences |> trainForIterations iterations }
    model

//=======================================================//



//=======================================================//

let tag (model: Model) (sentence:string list) = 
    
    // TODO: Update the prediction function to not require a weight tracker, and then write a wrapper with a weight tracker for training.
    let tempWeightTracker = 
        { Weights = model.Weights
          TrainingAccumulator = Map.empty
          InstanceLastSeen = Map.empty
          CurrentIteration = 0
          CurrentInstance = 0 }

    let convertToTaggingContext (sentence:string list) : (Word * Tag) list = 
        sentence   
            |> List.map (fun word -> 
                            (Word word, Tag "") )
            |> getTaggingContext

    sentence
        |> convertToTaggingContext
        |> List.windowed 5
        |> List.map getFeatures
        |> List.map (predictTagOnFeatures tempWeightTracker)
        |> List.map (fun prediction ->
                            (unwrapWord prediction.Word, unwrapTag prediction.PredictedTag)  )
        
        
let testTraining = train 5 trainingSet

testTraining

let sentence = 
    [ "my"; "name"; "is"; "rich"]

let testPrediction = sentence |> tag testTraining
testPrediction

    
     