import Control.Monad.RWS (gets)
-- Html.hs

-- * Types
newtype Structure = Structure String
newtype Html = Html String
type Title = String
type Body = String

-- * Tags

html_ :: Title -> Structure -> Html
html_ title content =
  Html
  ( tag "html"
    (tag "head" (tag "title" (escape title))
      <> tag "body" (getStructStr content)
    )
  )


p_ :: String -> Structure
p_ = Structure . tag "p"

h1_ :: String -> Structure
h1_ = Structure . tag "h1"

h2_ :: String -> Structure
h2_ = Structure . tag "h2"

list_ :: String -> [Structure] -> Structure
list_ list_type
  | list_type == "ul" = 
    Structure . tag "ul" . concat . map (tag "li" . getStructStr)
  | list_type == "ol" =
    Structure . tag "ul" . concat . map (tag "li" . getStructStr)

-- * Utils
tag :: String -> String -> String
tag t c
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