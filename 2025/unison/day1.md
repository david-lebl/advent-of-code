# Day 1

Code in DB:
```unison
decode: [Int] -> Int
decode input =
    let
        loop : Int -> Int -> [Int] -> Int
        loop dial zeros = cases
          [] -> zeros
          head +: tail -> 
            let
                moved = (dial + head) % +100
                if moved == +0 then loop moved (zeros + +1) tail
                else loop moved zeros tail
        loop +50 +0 input

program: Text -> {Exception} Int
program input = 
    unwrap: Text -> Optional a -> {Exception} a
    unwrap msg = cases
      Some value -> value
      None -> bug msg
    parse: Text -> Int
    parse line = 
      match Text.uncons line with
        Some(?R, xs) -> Int.fromText(xs) |> unwrap ("invalid number" ++ xs)
        Some(?L, xs) -> Int.fromText(xs) |> unwrap ("invalid number" ++ xs) |> Int.negate
        _ -> bug ("Failed to parse: " ++ line)
    let
        lines = splitOnNewline input
        moves = List.map parse lines
        decode moves
```

Main - example:
```unison
example: '{IO, Exception} ()
example _ = 
    result = program """
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    """
    printLine ("Password is: " ++ (Int.toText result))
```

Main - full data:
```unison
fullData: '{IO, Exception} ()
fullData _ = 
    inputData = readFileUtf8 (FilePath "../inputs/day1_input.txt")
    result = program inputData
    printLine ("Password is: " ++ (Int.toText result))
```

Running the examples from tmp file `scratch.u`
```ucm
run fullData
```