namespace Synsemantic.Tagger

module Domain = 

    type Word = Word of string
    let unwrapWord (Word w) = w

    //-------------------------------------------------------//

    type Tag = Tag of string
    let unwrapTag (Tag t) = t

    //-------------------------------------------------------//

    type Feature = Feature of string

    //-------------------------------------------------------//
    
    type WeightTracker = 
        { Weights:Map<Feature, Map<Tag, float>>
          TrainingAccumulator:Map<(Feature * Tag), float>
          InstanceLastSeen:Map<(Feature * Tag), int>
          CurrentIteration:int
          CurrentInstance:int }

    //-------------------------------------------------------//
    
    type Prediction =
        { Word:Word
          ActualTag:Tag
          PredictedTag:Tag
          Features:Feature list }

    //-------------------------------------------------------//
    
    type Model = 
        { Tags : Tag list
          Weights : Map<Feature, Map<Tag, float>> }


