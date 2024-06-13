-- Markup.hs

module Markup
    ( Document
    , Structure(..)
    , parse
    )
    where

import Numeric.Natural
import Data.Maybe ( maybeToList )

type Document = [Structure]

data Structure =
  Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
        [] -> maybeToList context

        -- h1
        ('#': ' ' : line) : rest ->
            maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
        
        -- ul
        ('-': ' ' : line) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just (UnorderedList (list <> [trim line]))) rest
            
            

trim :: String -> String
trim = unwords . words