namespace Synsemantic.Tagger

open Domain
open CoreOperations
open TrainingOperations

module Tagger =

    let train (iterations:int) (sentences:((Word * Tag) list) list) : Model =

        let model = 
            { Tags = sentences |> getTags
              Weights = sentences |> trainForIterations iterations }
        model

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
                                (unwrapWord prediction.Word, unwrapTag prediction.PredictedTag) )
