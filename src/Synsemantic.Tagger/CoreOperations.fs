namespace Synsemantic.Tagger

open Domain

module CoreOperations = 

    let getTaggingContext (taggedSentence:(Word * Tag) list) : (Word * Tag) list = 
        let capOne = (Word "_", Tag "_")
        let capTwo = (Word "__", Tag "__")
        let taggingContext = 
            [ capTwo; capOne ] @ taggedSentence @ [ capOne; capTwo ]
        taggingContext

    //=======================================================//

    let getFirstCharacter (word:Word) =  
        string (unwrapWord word).[0] 

    //=======================================================//
    
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

    //=======================================================//

    let getFeatures (context:(Word * Tag) list) =      

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

    //=======================================================//    
    
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
