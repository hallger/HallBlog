-- Html.hs

newtype Structure = Structure String

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . tag_ "h1"

tag_ :: String -> String -> String
tag_ t c
  = "<" <> t <> ">" <> c <> "</" <> t <> ">"
