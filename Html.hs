-- Html.hs

newtype Structure = Structure String

p_ :: String -> Structure
p_ = Structure . tag_ "p"

h1_ :: String -> Structure
h1_ = Structure . tag_ "h1"

tag_ :: String -> String -> String
tag_ t c
  = "<" <> t <> ">" <> c <> "</" <> t <> ">"

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        '_' -> "[c]"
    in
        concat . map escapeChar