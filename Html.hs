-- Html.hs

newtype Structure = Structure String

p_ :: String -> Structure
p_ = Structure . tag_ "p"

h1_ :: String -> Structure
h1_ = Structure . tag_ "h1"

h2_ :: String -> Structure
h2_ = Structure . tag_ "h2"

list_ :: String -> [Structure] -> Structure
list_ list_type
  | list_type == "ul" = 
    Structure . tag_ "ul" . concat . map (tag_ "li" . getStructStr)
  | list_type == "ol" =
    Structure . tag_ "ul" . concat . map (tag_ "li" . getStructStr)

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