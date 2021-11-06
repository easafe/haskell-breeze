{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Breeze (
    render,
    Options (..),
    defaultOptions
) where

import Constants (closeBracket, comma, createAttributeFunction, createElementFunction, dot, emptyText, knownAttributes, knownTags, openBracket, quote, space, unmatchedTag, voidTags, textFunction)
import qualified Data.HashMap.Strict as DHS
import qualified Data.List as DL
import qualified Data.Set as DS
import Data.Text (Text)
import qualified Data.Text as DT
import Text.Casing as TC
import Text.HTML.TagSoup (Tag (..))
import qualified Text.HTML.TagSoup as THT

data Options = Options
    { ignoreErrors :: Bool
    , elementModuleName :: Text
    , attributeModuleName :: Text
    }

defaultOptions :: Options
defaultOptions = Options
    { ignoreErrors = False
    , elementModuleName = "HE"
    , attributeModuleName = "HA"
    }

render :: Text -> Options -> Text
render rawHtml = renderHtml (THT.parseTags rawHtml)

renderHtml :: [Tag Text] -> Options -> Text
renderHtml tags Options{ignoreErrors, elementModuleName, attributeModuleName} = go tags [] emptyText
  where
    go tags stack running = case tags of
        [] -> if null stack then running else resultOrError stack running
        current : rest -> case current of
            TagOpen name attrs ->
                let isSelfClosingTag = DS.member name voidTags
                    s = if isSelfClosingTag then stack else name : stack
                    closing = if isSelfClosingTag then emptyText else space <> openBracket
                 in go rest s $ running <> renderTag name attrs <> closing
            TagClose name ->
                case DL.uncons stack of
                    Just (tag, s) | tag == name -> go rest s $ running <> closeBracket
                    _ -> resultOrError stack running <> closeBracket
            TagText text -> go rest stack $ running <> includeNode textFunction <> quoteIt text
            _ -> go rest stack running

    resultOrError stack running =
        if ignoreErrors
            then running
            else error . DT.unpack $ unmatchedTag <> DT.intercalate comma stack

    renderTag name attrs = dslName name <> space <> dslAttrs attrs

    dslName tag = elementModuleName <> dot <> if DS.member tag knownTags then tag else createElementFunction <> space <> tag

    dslAttrs attrs = openBracket <> DT.intercalate comma (map makeAttr attrs) <> closeBracket

    makeAttr (name, value) = case DHS.lookup name knownAttributes of
        Nothing -> includeAttribute createAttributeFunction <> name <> space <> value
        Just shouldBeQuoted -> includeAttribute (DT.pack . TC.camel $ DT.unpack name) <> if shouldBeQuoted then quoteIt value else value

    quoteIt value = quote <> value <> quote

    includeNode name = elementModuleName <> dot <> name <> space

    includeAttribute name = attributeModuleName <> dot <> name <> space