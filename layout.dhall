let Std = https://prelude.dhall-lang.org/package.dhall

let key_num = 8

let keys : List (List Natural) = Std.List.generate key_num (List Natural) (\(i: Natural) -> [i])

let chords : List (List Natural) = Std.List.concat (List Natural) (Std.List.generate key_num (List (List Natural)) (\(i: Natural) ->
    let base = i + 1
    in (Std.List.generate (Natural/subtract base key_num) (List Natural) (\(j: Natural) -> [i, base + j]))))

let chords : List (List Natural) = Std.List.concat (List Natural) [keys, chords]

let target : List Text = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

in { target = target, chords = chords }
