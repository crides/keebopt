let Std = https://prelude.dhall-lang.org/package.dhall sha256:21754b84b493b98682e73f64d9d57b18e1ca36a118b81b33d0a243de8455814b
let xml = Std.XML

let XML = xml.Type

let XMLement = xml.element
let XMLeaf = xml.leaf
let Attr = { mapKey : Text, mapValue : Text }

let svg = \(width : Natural) ->
          \(height : Natural) ->
          \(content : List XML) ->
          \(attrs : List Attr) ->
    XMLement { name = "svg"
             , attributes = toMap { viewBox = "0 0 ${Natural/show width} ${Natural/show height}"
                                  , xmlns = "http://www.w3.org/2000/svg"
                                  } # attrs
             , content
             }

let rect = \(x : Natural) ->
           \(y : Natural) ->
           \(width : Natural) ->
           \(height : Natural) ->
           \(attrs : List Attr) ->
    XMLeaf { name = "rect"
           , attributes = toMap { x = Natural/show x
                                , y = Natural/show y
                                , width = Natural/show width
                                , height = Natural/show height
                                } # attrs
           }

let text = \(x : Natural) ->
           \(y : Natural) ->
           \(text : Text) ->
    XMLement { name = "text"
             , attributes = toMap { x = Natural/show x
                                  , y = Natural/show y
                                  , class = "code"
                                  }
             , content = [xml.text text]
             }

let block_text = \(x : Natural) ->
                 \(y : Natural) ->
                 \(color : Text) ->
                 \(content : Text) ->
    [ rect (x + 1) (y + 1) 14 14 (toMap { rx = "2", fill = color })
    , text (x + 5) (y + 12) content
    ]

let Block = { fill: Text, text: Text }

let group = \(x : Natural) ->
            \(y : Natural) ->
            \(b0 : Block) ->
            \(b1 : Block) ->
            \(b2 : Block) ->
            \(b3 : Block) ->
            \(b4 : Block) ->
            \(b5 : Block) ->
            \(b6 : Block) ->
            \(b7 : Block) ->
    block_text x y b0.fill b0.text
    # block_text (x + 16) y b1.fill b1.text
    # block_text (x + 32) y b2.fill b2.text
    # block_text (x + 48) y b3.fill b3.text
    # block_text x (y + 16) b4.fill b4.text
    # block_text (x + 16) (y + 16) b5.fill b5.text
    # block_text (x + 32) (y + 16) b6.fill b6.text
    # block_text (x + 48) (y + 16) b7.fill b7.text

let image = { width = 256
            , height = 256
            }
-- Gruvbox
let black = "#282828"
let white = "#fbf1c7"
let red = "#cc241d"
let green = "#98971a"
let yellow = "#d79921"
let blue = "#458588"
let gray = "#a89984"

let style = XMLement { name = "style"
                     , attributes = xml.emptyAttributes
                     , content = [xml.text
                         ''
                         .code {
                             font: 12px Iosevka;
                             fill: ${white};
                         }
                         '']
                     }

in xml.render (svg image.width image.height
    (
        [ style
        , rect 0 0 image.width image.height (toMap { fill = black })
        ]
        # (group 16 16
            { fill = red, text = "E" }
            { fill = green, text = "N" }
            { fill = blue, text = "I" }
            { fill = yellow, text = "H" }
            { fill = black, text = "" }
            { fill = black, text = "" }
            { fill = black, text = "" }
            { fill = black, text = "" })
        # (group 96 16
            { fill = black, text = "" }
            { fill = black, text = "" }
            { fill = black, text = "" }
            { fill = black, text = "" })
            { fill = red, text = "O" }
            { fill = green, text = "T" }
            { fill = blue, text = "A" }
            { fill = yellow, text = "R" }
        # (group 16 56
            { fill = red, text = "G" }
            { fill = green, text = "M" }
            { fill = blue, text = "," }
            { fill = yellow, text = "." }
            { fill = red, text = "" }
            { fill = green, text = "" }
            { fill = blue, text = "" }
            { fill = yellow, text = "" })
        # (group 96 56
            { fill = red, text = "X" }
            { fill = green, text = "F" }
            { fill = green, text = "" }
            { fill = red, text = "" }
            { fill = blue, text = "" }
            { fill = yellow, text = "" }
            { fill = yellow, text = "P" }
            { fill = blue, text = "K" })
        # (group 16 96
            { fill = red, text = "L" }
            { fill = red, text = "" }
            { fill = green, text = "" }
            { fill = green, text = "!" }
            { fill = blue, text = "S" }
            { fill = blue, text = "" }
            { fill = yellow, text = "" }
            { fill = yellow, text = "?" })
        # (group 96 96
            { fill = red, text = "D" }
            { fill = green, text = "" }
            { fill = red, text = "" }
            { fill = green, text = "J" }
            { fill = blue, text = "U" }
            { fill = yellow, text = "" }
            { fill = blue, text = "" }
            { fill = yellow, text = "Q" })
        # (group 16 136
            { fill = red, text = "V" }
            { fill = blue, text = "" }
            { fill = white, text = "" }
            { fill = white, text = "" }
            { fill = blue, text = "C" }
            { fill = red, text = "" }
            { fill = white, text = "" }
            { fill = white, text = "" })
        # (group 96 136
            { fill = white, text = "" }
            { fill = green, text = "B" }
            { fill = white, text = "" }
            { fill = white, text = "" }
            { fill = white, text = "" }
            { fill = white, text = "" }
            { fill = green, text = "" }
            { fill = white, text = "" })
        # (group 16 176
            { fill = red, text = "Y" }
            { fill = green, text = "Z" }
            { fill = blue, text = "" }
            { fill = white, text = "" }
            { fill = blue, text = "W" }
            { fill = white, text = "" }
            { fill = red, text = "" }
            { fill = green, text = "" })
    )
    (toMap { fill = gray }))
