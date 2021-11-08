{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Breeze (
    render,
    Options (..)
) where

import Constants (
    attributeFreeTags,
    childFreeTags,
    closeBracket,
    comma,
    createAttributeFunction,
    createElementFunction,
    dot,
    emptyText,
    knownAttributes,
    knownTags,
    openBracket,
    quote,
    space,
    textFunction,
    unmatchedTag, viewFunction, moduleName, newline, importList
 )
import qualified Data.HashMap.Strict as DHS
import qualified Data.List as DL
import qualified Data.Set as DS
import Data.Text (Text)
import qualified Data.Text as DT
import Text.Casing as TC
import Text.HTML.TagSoup (Tag (..))
import Text.HTML.TagSoup.Tree (TagTree (..))
import qualified Text.HTML.TagSoup.Tree as THTT

-- | Options for the rendering process
data Options = Options
    { input :: Text
    , ignoreErrors :: Bool
    , standaloneModule :: Bool
    , elementModuleName :: Text
    , attributeModuleName :: Text
    , outputFile :: Maybe FilePath
    }

-- | Renders HTML to Flame markup
render :: Text -> Options -> Text
render rawHtml options = renderModule options <> renderHtml (THTT.parseTree rawHtml) options

{- |
    Renders a HTML tree into text

    No effort is made to use sugar syntax, all nodes are rendered as HE.name [] []
-}
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
                        | DS.member name childFreeTags -> renderTag name attributes -- tags like input cannot have child nodes
                        | otherwise ->
                            if ignoreErrors
                                then renderTag name attributes
                                else error . DT.unpack $ unmatchedTag <> name
                    TagText text -> includeNode textFunction <> quoteIt text
                    _ -> emptyText

    renderTag name attributes = dslName name <> dslAttributes name attributes

    dslName tag
        | DS.member tag knownTags = includeNode tag
        | otherwise = includeNode $ createElementFunction <> space <> quoteIt tag -- include unknown tags with HE.createElement
    dslAttributes name attributes
        | DS.member name attributeFreeTags = emptyText
        | otherwise = openBracket <> DT.intercalate comma (map makeAttribute attributes) <> closeBracket

    makeAttribute (name, value) = case DHS.lookup name knownAttributes of
        Nothing -> includeAttribute createAttributeFunction <> quoteIt name <> space <> quoteIt value -- include unknown attributes with HA.createAttributes
        Just shouldBeQuoted -> includeAttribute (DT.pack . TC.camel $ DT.unpack name) <> if shouldBeQuoted then quoteIt value else value -- quotes should not be added for boolean or numeric attributes
    quoteIt value = quote <> value <> quote

    includeNode name = elementModuleName <> dot <> name <> space

    includeAttribute name = attributeModuleName <> dot <> name <> space

-- | Includes code for a standalone module
renderModule :: Options -> Text
renderModule Options{standaloneModule, elementModuleName, attributeModuleName}
    | standaloneModule = moduleName <> newline <> importList elementModuleName attributeModuleName <> newline <> viewFunction
    | otherwise = emptyText