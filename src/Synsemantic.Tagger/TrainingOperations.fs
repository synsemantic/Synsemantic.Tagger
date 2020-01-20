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