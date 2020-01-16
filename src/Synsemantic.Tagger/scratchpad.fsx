
let getTrainingContext (taggedSentence:(string * string) List) : (string * string) List = 
    let trainingContext = 
        [ ("__", "__"); ("_", "_") ] @ taggedSentence @ [ ("_", "_"); ("__", "__") ]
    trainingContext

let getFirstCharacter (word:string) = 
    string word.[0]

let getSuffix (word:string) =
    let suffix (word:string) =
        let startOfSuffix = (String.length word) - 3
        let endOfWord = (String.length word) - 1
        word.[startOfSuffix .. endOfWord]
    match (String.length word) with
        | l when l <= 3 -> word
        | l when l >= 4 -> word |> suffix
        | _ -> "Something went terribly wrong here..."

let getFeatures (context:(string * string) List) : Map<string, float> =
    let featuresMap = 
        [ "bias", 1.0
          "word_" + fst context.[2], 1.0
          "word_suffix_" + (fst context.[2] |> getSuffix), 1.0
          "word_first_letter_" + (fst context.[2] |> getFirstCharacter), 1.0
          "previous_word_" + fst context.[1], 1.0
          "previous_word_suffix_" + (fst context.[1] |> getSuffix), 1.0
          "previous_word_tag_" + snd context.[1], 1.0
          "second_previous_word_" + fst context.[0], 1.0
          "second_previous_word_tag_" + snd context.[0], 1.0
          "following_word_" + fst context.[3], 1.0
          "following_word_suffix_" + (fst context.[3] |> getSuffix), 1.0
          "second_following_word_" + fst context.[4], 1.0
          "word_tag_plus_second_previous_word_tag_" + snd context.[2] + "_" + snd context.[0], 1.0  
          "previous_word_tag_plus_word_" + (snd context.[1] + "_" + fst context.[2]), 1.0 ]
        |> Map.ofList
    featuresMap

let y = [ ("my", "MY"); ("name", "NAME"); ("is", "IS"); ("rich", "RICH"); ("what", "WHAT"); ("is", "IS"); ("your", "YOUR"); ("name", "NAME") ]

y 
    |> getTrainingContext
    |> List.windowed 5
    |> List.map getFeatures

//make the dictionary of unambiguous tags (every instance of word has same tag)

//track the counts of word/tag occurrence
// use a frequency threshold and an ambiguity threshold

let getWordTagPairCounts (sentences:((string * string) List) List) =
    let reducer = fun x y -> x @ y
    let combinedLists = sentences |> List.reduce reducer

    let mutable wordTagCountMap = new 

    combinedLists
   
    // for each sentence -> 
    //end with a list of dictionaries (word dictionary<tag, count))

let listOfLists = [ 
          [ ("my", "MY"); ("name", "NAME") ];
          [ ("is", "IS"); ("rich", "RICH") ];
          [ ("what", "WHAT"); ("is", "IS") ]; 
          [ ("your", "YOUR"); ("name", "NAME") ] ]

let listsEverywhere = listOfLists |> getWordTagPairCounts

listsEverywhere


(*def _make_tagdict(self, sentences):
    """
    Make a tag dictionary for single-tag words.
    :param sentences: A list of list of (word, tag) tuples.
    """
    counts = defaultdict(lambda: defaultdict(int))

    //ONE FUNCTION
    for sentence in sentences:
        self._sentences.append(sentence)
        for word, tag in sentence:
            counts[word][tag] += 1
            self.classes.add(tag)


    freq_thresh = 20
    ambiguity_thresh = 0.97

    //ONE FUNCTION
    for word, tag_freqs in counts.items():
        tag, mode = max(tag_freqs.items(), key=lambda item: item[1])
        n = sum(tag_freqs.values())
        # Don't add rare words to the tag dictionary
        # Only add quite unambiguous words
        if n >= freq_thresh and (mode / n) >= ambiguity_thresh:
            self.tagdict[word] = tag*)



