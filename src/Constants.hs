{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as DHS
import Data.Set (Set)
import qualified Data.Set as DS
import Data.Text (Text)

type ShouldValueBeQuoted = Bool

newline :: Text
newline = "\n"

moduleName :: Text
moduleName = "module Breeze.Output where" <> newline

importList :: Text -> Text -> Text
importList elementsName attributesName = "import Flame (Html)" <> newline <> "import Flame.Html.Element as " <> elementsName <> newline <> "import Flame.Html.Attribute as " <> attributesName <> newline

viewFunction :: Text
viewFunction = "view :: forall model message. model -> Html message" <> newline <> "view model = "

openBracket :: Text
openBracket = "["

closeBracket :: Text
closeBracket = "]"

dot :: Text
dot = "."

comma :: Text
comma = ", "

quote :: Text
quote = "\""

space :: Text
space = " "

textFunction :: Text
textFunction = "text"

emptyText :: Text
emptyText = ""

unmatchedTag :: Text
unmatchedTag = "Unmatched tag: "

createElementFunction :: Text
createElementFunction = "createElement"

createAttributeFunction :: Text
createAttributeFunction = "createAttribute"

childFreeTags :: Set Text
childFreeTags =
    DS.fromList
        [ "area"
        , "base"
        , "br"
        , "doctype"
        , "col"
        , "command"
        , "embed"
        , "hr"
        , "img"
        , "input"
        , "keygen"
        , "link"
        , "meta"
        , "param"
        , "source"
        , "track"
        , "wbr"
        ]

attributeFreeTags :: Set Text
attributeFreeTags = DS.fromList ["br", "hr"]

knownTags :: Set Text
knownTags =
    DS.fromList
        [ "a"
        , "address"
        , "animate"
        , "animateColor"
        , "animateMotion"
        , "animateTransform"
        , "area"
        , "article"
        , "aside"
        , "audio"
        , "b"
        , "base"
        , "bdi"
        , "bdo"
        , "blockquote"
        , "body"
        , "br"
        , "button"
        , "canvas"
        , "caption"
        , "circle"
        , "cite"
        , "clipPath"
        , "code"
        , "col"
        , "colgroup"
        , "colorProfile"
        , "cursor"
        , "datalist"
        , "dd"
        , "defs"
        , "del"
        , "desc"
        , "details"
        , "dfn"
        , "dialog"
        , "discard"
        , "div"
        , "dl"
        , "doctype"
        , "dt"
        , "ellipse"
        , "em"
        , "embed"
        , "feBlend"
        , "feColorMatrix"
        , "feComponentTransfer"
        , "feComposite"
        , "feConvolveMatrix"
        , "feDiffuseLighting"
        , "feDisplacementMap"
        , "feDistantLight"
        , "feDropShadow"
        , "feFlood"
        , "feFuncA"
        , "feFuncB"
        , "feFuncG"
        , "feFuncR"
        , "feGaussianBlur"
        , "feImage"
        , "feMerge"
        , "feMergeNode"
        , "feMorphology"
        , "feOffset"
        , "fePointLight"
        , "feSpecularLighting"
        , "feSpotLight"
        , "feTile"
        , "feTurbulence"
        , "fieldset"
        , "figure"
        , "filter"
        , "font"
        , "fontFace"
        , "fontFaceFormat"
        , "fontFaceName"
        , "fontFaceSrc"
        , "fontFaceUri"
        , "footer"
        , "foreignObject"
        , "form"
        , "g"
        , "glyph"
        , "glyphRef"
        , "h1"
        , "h2"
        , "h3"
        , "h4"
        , "h5"
        , "h6"
        , "hatch"
        , "hatchpath"
        , "head"
        , "header"
        , "hgroup"
        , "hkern"
        , "hr"
        , "html"
        , "i"
        , "iframe"
        , "image"
        , "input"
        , "ins"
        , "keygen"
        , "label"
        , "legend"
        , "li"
        , "line"
        , "linearGradient"
        , "main"
        , "map"
        , "mark"
        , "marker"
        , "mask"
        , "menu"
        , "menuitem"
        , "mesh"
        , "meshgradient"
        , "meshpatch"
        , "meshrow"
        , "metadata"
        , "meter"
        , "missingGlyph"
        , "mpath"
        , "nav"
        , "noscript"
        , "object"
        , "ol"
        , "optgroup"
        , "option"
        , "output"
        , "p"
        , "param"
        , "path"
        , "pattern"
        , "polygon"
        , "polyline"
        , "pre"
        , "progress"
        , "q"
        , "radialGradient"
        , "rb"
        , "rect"
        , "rp"
        , "rt"
        , "rtc"
        , "ruby"
        , "s"
        , "script"
        , "section"
        , "select"
        , "set"
        , "small"
        , "solidcolor"
        , "source"
        , "span"
        , "stop"
        , "strong"
        , "style"
        , "sub"
        , "summary"
        , "sup"
        , "svg"
        , "switch"
        , "symbol"
        , "table"
        , "tbody"
        , "td"
        , "template"
        , "textarea"
        , "textPath"
        , "tfoot"
        , "th"
        , "thead"
        , "time"
        , "tr"
        , "track"
        , "tref"
        , "tspan"
        , "u"
        , "ul"
        , "unknown"
        , "use"
        , "var"
        , "video"
        , "view"
        , "vkern"
        , "wbr"
        ]

knownAttributes :: HashMap Text ShouldValueBeQuoted
knownAttributes =
    DHS.fromList
        [ ("accent-height", True)
        , ("accept", True)
        , ("accept-charset", True)
        , ("access-key", True)
        , ("accumulate", True)
        , ("action", True)
        , ("additive", True)
        , ("align", True)
        , ("alignment-baseline", True)
        , ("alt", True)
        , ("ascent", False)
        , ("autocomplete", True)
        , ("autofocus", False)
        , ("autoplay", False)
        , ("azimuth", False)
        , ("base-frequency", True)
        , ("base-profile", True)
        , ("baseline-shift", True)
        , ("begin", True)
        , ("bias", False)
        , ("calc-mode", True)
        , ("charset", True)
        , ("checked", False)
        , ("class", True)
        , ("clip-path-attr", True)
        , ("clip-path-units", True)
        , ("clip-rule", True)
        , ("color", True)
        , ("color-interpolation", True)
        , ("color-interpolation-filters", True)
        , ("color-profile-attr", True)
        , ("color-rendering", True)
        , ("cols", False)
        , ("colspan", False)
        , ("content", True)
        , ("content-editable", True)
        , ("content-script-type", True)
        , ("content-style-type", True)
        , ("contextmenu", True)
        , ("controls", False)
        , ("coords", True)
        , ("create-attribute-name", True)
        , ("create-attribute-type", True)
        , ("cursor-attr", True)
        , ("cx", True)
        , ("cy", True)
        , ("d", True)
        , ("datetime", True)
        , ("default", False)
        , ("diffuse-constant", True)
        , ("dir", True)
        , ("direction", True)
        , ("disabled", False)
        , ("display", True)
        , ("divisor", False)
        , ("dominant-baseline", True)
        , ("download", True)
        , ("download-as", True)
        , ("draggable", True)
        , ("dropzone", True)
        , ("dur", True)
        , ("dx", True)
        , ("dy", True)
        , ("edge-mode", True)
        , ("elevation", False)
        , ("enctype", True)
        , ("end", True)
        , ("external-resources-required", True)
        , ("fill", True)
        , ("fill-opacity", True)
        , ("fill-rule", True)
        , ("filter-attr", True)
        , ("filter-units", True)
        , ("flood-color", True)
        , ("flood-opacity", True)
        , ("font-family", True)
        , ("font-size", True)
        , ("font-size-adjust", True)
        , ("font-stretch", True)
        , ("font-style", True)
        , ("font-variant", True)
        , ("font-weight", True)
        , ("for", True)
        , ("fr", False)
        , ("from", True)
        , ("fx", True)
        , ("fy", True)
        , ("gradient-transform", True)
        , ("gradient-units", True)
        , ("headers", True)
        , ("height", True)
        , ("hidden", False)
        , ("href", True)
        , ("hreflang", True)
        , ("id", True)
        , ("image-rendering", True)
        , ("in", True)
        , ("in-2", True)
        , ("inner-html", True)
        , ("is-map", True)
        , ("itemprop", True)
        , ("k-1", True)
        , ("k-2", True)
        , ("k-3", True)
        , ("k-4", True)
        , ("kernel-matrix", True)
        , ("kernel-unit-length", True)
        , ("kerning", True)
        , ("key-splines", True)
        , ("key-times", True)
        , ("kind", True)
        , ("lang", True)
        , ("length-adjust", True)
        , ("letter-spacing", True)
        , ("lighting-color", True)
        , ("limiting-cone-angle", True)
        , ("list", True)
        , ("local", True)
        , ("loop", False)
        , ("manifest", True)
        , ("marker-end", True)
        , ("marker-height", True)
        , ("marker-mid", True)
        , ("marker-start", True)
        , ("marker-units", True)
        , ("marker-width", True)
        , ("mask-attr", True)
        , ("mask-content-units", True)
        , ("mask-units", True)
        , ("max", True)
        , ("maxlength", False)
        , ("media", True)
        , ("method", True)
        , ("min", True)
        , ("minlength", False)
        , ("mode", True)
        , ("multiple", False)
        , ("name", True)
        , ("no-validate", True)
        , ("num-octaves", True)
        , ("opacity", True)
        , ("operator", True)
        , ("order", True)
        , ("overflow", True)
        , ("overline-position", True)
        , ("overline-thickness", True)
        , ("paint-order", True)
        , ("path-length", True)
        , ("pattern", True)
        , ("pattern-content-units", True)
        , ("pattern-transform", True)
        , ("pattern-units", True)
        , ("ping", True)
        , ("placeholder", True)
        , ("pointer-events", True)
        , ("points", True)
        , ("points-at-x", True)
        , ("points-at-y", True)
        , ("points-at-z", True)
        , ("poster", True)
        , ("preload", True)
        , ("preserve-alpha", True)
        , ("preserve-aspect-ratio", True)
        , ("primitive-units", True)
        , ("pubdate", True)
        , ("r", True)
        , ("radius", True)
        , ("read-only", True)
        , ("ref-x", True)
        , ("ref-y", True)
        , ("rel", True)
        , ("repeat-count", True)
        , ("repeat-dur", True)
        , ("required", False)
        , ("required-features", True)
        , ("restart", True)
        , ("result", True)
        , ("reversed", False)
        , ("rows", False)
        , ("rowspan", False)
        , ("rx", True)
        , ("ry", True)
        , ("sandbox", True)
        , ("scale", False)
        , ("scope", True)
        , ("seed", False)
        , ("selected", False)
        , ("shape", True)
        , ("shape-rendering", True)
        , ("size", False)
        , ("specular-constant", True)
        , ("specular-exponent", True)
        , ("spellcheck", False)
        , ("src", True)
        , ("srcdoc", True)
        , ("srclang", True)
        , ("start", False)
        , ("std-deviation", True)
        , ("step", True)
        , ("stitch-tiles", True)
        , ("stop-color", True)
        , ("stop-opacity", True)
        , ("strikethrough-position", True)
        , ("strikethrough-thickness", True)
        , ("stroke", True)
        , ("stroke-dasharray", True)
        , ("stroke-dashoffset", True)
        , ("stroke-linecap", True)
        , ("stroke-linejoin", True)
        , ("stroke-miterlimit", True)
        , ("stroke-opacity", True)
        , ("stroke-width", True)
        , ("style", False)
        , ("surface-scale", True)
        , ("tabindex", False)
        , ("target", True)
        , ("target-x", True)
        , ("target-y", True)
        , ("text-anchor", True)
        , ("text-decoration", True)
        , ("text-length", True)
        , ("text-rendering", True)
        , ("title", True)
        , ("transform", True)
        , ("type", True)
        , ("underline-position", True)
        , ("underline-thickness", True)
        , ("use-map", True)
        , ("value", True)
        , ("values", True)
        , ("vector-effect", True)
        , ("version", False)
        , ("view-box", True)
        , ("visibility", True)
        , ("width", True)
        , ("word-spacing", True)
        , ("wrap", True)
        , ("writing-mode", True)
        , ("x", True)
        , ("x-1", True)
        , ("x-2", True)
        , ("x-channel-selector", True)
        , ("y", True)
        , ("y-1", True)
        , ("y-2", True)
        , ("y-channel-selector", True)
        , ("onBlur", False)
        , ("onChange", False)
        , ("onCheck", False)
        , ("onClick", False)
        , ("onContextmenu", False)
        , ("onDblclick", False)
        , ("onDrag", False)
        , ("onDragend", False)
        , ("onDragenter", False)
        , ("onDragleave", False)
        , ("onDragover", False)
        , ("onDragstart", False)
        , ("onDrop", False)
        , ("onError", False)
        , ("onFocus", False)
        , ("onFocusin", False)
        , ("onFocusout", False)
        , ("onInput", False)
        , ("onKeydown", False)
        , ("onKeypress", False)
        , ("onKeyup", False)
        , ("onMousedown", False)
        , ("onMouseenter", False)
        , ("onMouseleave", False)
        , ("onMousemove", False)
        , ("onMouseout", False)
        , ("onMouseover", False)
        , ("onMouseup", False)
        , ("onReset", False)
        , ("onScroll", False)
        , ("onSelect", False)
        , ("onWheel", False)
        ]
