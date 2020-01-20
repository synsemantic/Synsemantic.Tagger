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
