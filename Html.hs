-- Html.hs

newtype Structure = Structure String

p_ :: String -> Structure
p_ = Structure . tag_ "p"

h1_ :: String -> Structure
h1_ = Structure . tag_ "h1"

h2_ :: String -> Structure
h2_ Structure . tag_ "h2"

ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructStr)

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

getStructStr :: Structure -> String
getStructStr struct =
  case struct of
    Structure str -> str