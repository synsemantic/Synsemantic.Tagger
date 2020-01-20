namespace Synsemantic.Tagger

open System
open Domain
open CoreOperations

module TrainingOperations = 

    let getTags (sentences:((Word * Tag) list) list) =
        let reducer listOne listTwo = listOne @ listTwo
    
        let tags = 
            sentences
                |> List.reduce reducer
                |> List.unzip
    
        (snd tags) |> List.distinct

    //=======================================================//
    // From https://www.rosettacode.org/wiki/Knuth_shuffle#F.23

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