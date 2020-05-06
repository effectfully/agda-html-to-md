{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor
import           Data.Maybe
import           Data.Text       as Text
import           Data.Text.IO    as Text
import           Text.HTMLEntity (decode)

unsafeBreakDrop :: Text -> Text -> (Text, Text)
unsafeBreakDrop needle haystack =
    fromMaybe (error $ Text.unpack needle) . stripPrefix needle <$> breakOn needle haystack

-- {_}({style}@<style{_}</style>){_}<span class="default">{rest}
-- becomes
-- <pre>{style}</pre>{rest}
extractStyle :: Text -> Text
extractStyle
    = uncurry mappend
    . bimap
        (\style -> "<pre>" <> style <> "</style></pre>")
        (snd . breakOn "<span class=\"default\">")
    . unsafeBreakDrop "</style>"
    . snd
    . breakOn "<style"

transformAfter :: Text -> (Text -> Text) -> Text -> Text
transformAfter after trans text = case splitOn after text of
    []     -> error "The impossible happened"
    t : ts -> Text.concat $ t : Prelude.map trans ts

-- <span class="default">{text}</span>
-- becomes
-- {text}
formatNormalText :: Text -> Text
formatNormalText = transformAfter start extractText where
    start = "<span class=\"default\">"

    extractText part = either error id (decode text) <> rest where
        (text, rest) = unsafeBreakDrop "</span>" part

-- <span class="comment-delimiter">```agda{_}</span>{code}<span class="comment-delimiter">```{_}</span>
-- becomes
-- <pre>{code}</pre>
formatAgdaCode :: Text -> Text
formatAgdaCode = transformAfter start extractCode where
    start = "<span class=\"comment-delimiter\">```agda"

    extractCode part = Text.concat ["<pre>", code, "</pre>", afterSpan rest] where
        afterSpan = snd . unsafeBreakDrop "</span>"
        (code, rest) = unsafeBreakDrop "<span class=\"comment-delimiter\">```" $ afterSpan part

htmlToMd :: Text -> Text
htmlToMd = formatAgdaCode . formatNormalText . extractStyle

main = Text.interact htmlToMd
