{-# LANGUAGE OverloadedStrings #-}
-- | Web and e-mail addresses in a message: like
-- GitHub's autolinks, at the start of a word or after an opening bracket,
-- quote or emphasis mark.
module Tox.Android.Links
  ( findLinks
  ) where

import           Data.Char (isAlphaNum, isAscii, isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Each link as it is written, and the address it opens.
findLinks :: Text -> [(Text, Text)]
findLinks = go True
  where
    go starts text
      | Text.null text = []
      | starts, Just (candidate, rest) <- web text <> email text =
          maybe id (:) (link candidate) (go False rest)
      | otherwise = let c = Text.head text in go (opens c) (Text.tail text)
    opens c = isSpace c || c `elem` ("([{<\"'*_~" :: String)

-- | A web address from here, up to what ends it.
web :: Text -> Maybe (Text, Text)
web text
  | any (`Text.isPrefixOf` Text.toLower text) ["http://", "https://", "www."] =
      Just (Text.break (\c -> isSpace c || c `elem` ("<>\"" :: String)) text)
  | otherwise = Nothing

-- | An e-mail address from here: a name, @, and two or more labels.
email :: Text -> Maybe (Text, Text)
email text = do
  let (name, afterName) = Text.span (\c -> word c || c `elem` (".+-" :: String)) text
  rest <- Text.stripPrefix "@" afterName
  let labels = takeWhile (not . Text.null) (Text.splitOn "." (Text.takeWhile (\c -> word c || c `elem` ("-." :: String)) rest))
      domain = Text.intercalate "." labels
  if Text.null name || length labels < 2 then Nothing
    else Just (Text.splitAt (Text.length name + 1 + Text.length domain) text)

link :: Text -> Maybe (Text, Text)
link candidate
  | Nothing <- web candidate =
      if Text.last candidate `elem` ("-_" :: String) then Nothing else Just (candidate, "mailto:" <> candidate)
  | hosted trimmed = Just (trimmed, if scheme then trimmed else "https://" <> trimmed)
  | otherwise = Nothing
  where
    trimmed = trim candidate
    scheme = any (`Text.isPrefixOf` Text.toLower trimmed) ["http://", "https://"]

-- | Without punctuation at the end, or closing brackets with no opening one.
trim :: Text -> Text
trim l = case Text.unsnoc l of
  Just (rest, c)
    | c `elem` ("?!.,:;*_~'" :: String) -> trim rest
    | Just o <- lookup c [(')', '('), (']', '['), ('}', '{')]
    , Text.count (Text.singleton c) l > Text.count (Text.singleton o) l -> trim rest
  _ -> l

-- | Whether its host has two or more labels, the last two without
-- underscores.
hosted :: Text -> Bool
hosted l = length labels >= 2 && all (\x -> not (Text.null x) && Text.all host x) labels
  && all (Text.all (/= '_')) (drop (length labels - 2) labels)
  where
    bare = maybe l snd (lookup True [(p `Text.isPrefixOf` Text.toLower l, Text.splitAt (Text.length p) l) | p <- ["https://", "http://"]])
    labels = Text.splitOn "." (Text.takeWhile (`notElem` ("/?#:" :: String)) bare)
    host c = word c || c == '-'

-- | What a regular expression's \w takes.
word :: Char -> Bool
word c = isAscii c && (isAlphaNum c || c == '_')
