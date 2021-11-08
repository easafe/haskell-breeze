{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Breeze (Options (..))
import qualified Breeze as B
import Constants (acceptedFileExtensions)
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as OA

parser :: Parser Options
parser =
    Options
        <$> OA.argument OA.str (OA.metavar "INPUT")
        <*> OA.switch
            ( OA.long "ignore-errors"
                <> OA.short 'i'
                <> OA.help "Ignore parsing errors. Default: stop at parsing errors"
            )
        <*> OA.switch
            ( OA.long "standalone-module"
                <> OA.short 's'
                <> OA.help "Output a module with import list and view function. Default: output only generated markup"
            )
        <*> OA.strOption
            ( OA.long "element-module-name"
                <> OA.short 'e'
                <> OA.help "Name to import Flame.Html.Element as. Default: HE"
                <> OA.value "HE"
                <> OA.metavar "STRING"
            )
        <*> OA.strOption
            ( OA.long "attribute-module-name"
                <> OA.short 'a'
                <> OA.help "Name to import Flame.Html.Attribute as. Default: HA"
                <> OA.value "HA"
                <> OA.metavar "STRING"
            )
        <*> OA.optional
            ( OA.strOption $
                OA.long "output-file"
                    <> OA.short 'o'
                    <> OA.help "File to save output as. Default: print to command line"
                    <> OA.metavar "FILE"
            )

main :: IO ()
main = do
    options@Options{input, outputFile} <- OA.execParser flags
    rawHtml <- readInput input
    let output = B.render rawHtml options
    case outputFile of
        Nothing -> DTI.putStrLn output
        Just file -> DTI.writeFile file output
  where
    flags =
        OA.info
            (parser <**> OA.helper)
            ( OA.fullDesc
                <> OA.header "breeze - convert HTML into purescript-flame DSL"
                <> OA.progDesc "Where INPUT can be either a HTML string or an input file (.html|.htm|.xml)"
            )

    -- the first argument can be either a file path or a html snippet
    readInput input
        | DS.member (fileExtension input) acceptedFileExtensions = DTI.readFile $ DT.unpack input
        | otherwise = pure input

    fileExtension = DT.reverse . DT.takeWhile ('.' /=) . DT.reverse