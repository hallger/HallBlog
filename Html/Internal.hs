-- Html/Internal.hs
module Html.Internal where
-- * Types
newtype Structure = Structure String
newtype Html = Html String
type Title = String
type Body = String

-- * html
html_ :: Title -> Structure -> Html
html_ title content =
  Html
  ( tag "html"
    (tag "head" (tag "title" (escape title))
      <> tag "body" (getStructStr content)
    )
  )

-- * Tags
p_ :: String -> Structure
p_ = Structure . tag "p" . escape

h1_ :: String -> Structure
h1_ = Structure . tag "h1" . escape

h2_ :: String -> Structure
h2_ = Structure . tag "h2" . escape

list_ :: String -> [Structure] -> Structure
list_ list_type
  | list_type == "ul" =
    Structure . tag "ul" . concatMap (tag "li" . getStructStr)
  | list_type == "ol" =
    Structure . tag "ul" . concatMap (tag "li" . getStructStr)

ul_ :: [Structure] -> Structure
ul_ = 
    Structure . tag "ul" . concatMap (tag "li" . getStructStr)

ol_ :: [Structure] -> Structure
ol_ = 
    Structure . tag "ol" . concatMap (tag "li" . getStructStr)
code_ :: String -> Structure
code_ = Structure . tag "pre" . escape

-- * Utils
tag :: String -> String -> String
tag t c
  = "<" <> t <> ">" <> c <> "</" <> t <> ">"

render :: Html -> String
render html =
  case html of
    Html str -> str

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
        concatMap escapeChar

getStructStr :: Structure -> String
getStructStr struct =
  case struct of
    Structure str -> str

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructStr c1 <> getStructStr c2)