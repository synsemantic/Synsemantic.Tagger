namespace Synsemantic.Tagger

module Domain = 

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


