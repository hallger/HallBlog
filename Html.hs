-- Html.hs

newtype Structure = Structure String

h1_ :: String -> Structure
h1_ = Structure . tag "h1"

tag_ :: String -> String -> String
tag t c
  = "<" <> t <> ">" <> c <> "</" <> t <> ">"
