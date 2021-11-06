{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Breeze (
    render,
    Options (..),
    defaultOptions
) where

import Constants (closeBracket, comma, createAttributeFunction, createElementFunction, dot, emptyText, knownAttributes, knownTags, openBracket, quote, space, unmatchedTag, textFunction, childFreeTags)
import qualified Data.HashMap.Strict as DHS
import qualified Data.List as DL
import qualified Data.Set as DS
import Data.Text (Text)
import qualified Data.Text as DT
import Text.Casing as TC
import Text.HTML.TagSoup (Tag (..))
import Text.HTML.TagSoup.Tree (TagTree(..))
import qualified Text.HTML.TagSoup.Tree as THTT

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
render rawHtml = renderHtml (THTT.parseTree rawHtml)

--restore all test and fixes
renderHtml :: [TagTree Text] -> Options -> Text
renderHtml tree Options{ignoreErrors, elementModuleName, attributeModuleName} = flattenTree tree
  where
    flattenTree = DT.intercalate comma . map go
    go current =
        case current of
            TagBranch name attributes children ->
                renderTag name attributes <> space <> openBracket <> flattenTree children <> closeBracket
            TagLeaf tag ->
                case tag of
                    TagOpen name attributes
                        | DS.member name childFreeTags -> renderTag name attributes
                        | otherwise ->
                            if ignoreErrors
                                then emptyText
                                else error . DT.unpack $ unmatchedTag <> name
                    TagText text -> includeNode textFunction <> quoteIt text
                    _ -> emptyText

    renderTag name attributes = dslName name <> space <> dslAttributes attributes

    dslName tag = elementModuleName <> dot <> if DS.member tag knownTags then tag else createElementFunction <> space <> tag

    dslAttributes attributes = openBracket <> DT.intercalate comma (map makeAttribute attributes) <> closeBracket

    makeAttribute (name, value) = case DHS.lookup name knownAttributes of
        Nothing -> includeAttribute createAttributeFunction <> name <> space <> value
        Just shouldBeQuoted -> includeAttribute (DT.pack . TC.camel $ DT.unpack name) <> if shouldBeQuoted then quoteIt value else value

    quoteIt value = quote <> value <> quote

    includeNode name = elementModuleName <> dot <> name <> space

    includeAttribute name = attributeModuleName <> dot <> name <> space