namespace GasdospelaToSMV

module Bit64 =
    let toString (word:uint64) =
        let rec loop =
            function
            | 0UL -> ""
            | w ->
                loop (w/2UL) +
                    if w &&& 1UL = 1UL
                    then "1"
                    else "0"
        if word = 0UL
        then "0"
        else loop word

    let get (index:int) (size:int) (word:uint64) =
        let mask = ((1UL <<< size) - 1UL) <<< index
        (mask &&& word) >>> index

    let getSafe (index:int) (size:int) (word:uint64) =
        if index < 0 || size <= 0
        then failwith "arg negative or zero"
        if index + size > 64
        then failwith "outside bounds"        
        get index size word

    let set (index:int) (size:int) (value:uint64) (word:uint64) =
        let mask = ~~~(((1UL <<< size) - 1UL) <<< index)
        let existing = word &&& mask
        existing ||| (value <<< index) 

    let setSafe (index:int) (size:int) (value:uint64) (word:uint64) =
        if index < 0 || size <= 0
        then failwith "arg negative or zero"
        if index + size > 64
        then failwith "outside bounds"        
        if value > (1UL <<< size)
        then failwith "value larger than size"
        set index size value word
