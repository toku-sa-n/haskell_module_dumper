module Main
  ( main
  ) where

import           GHC.Data.EnumSet
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Hs.Dump
import qualified GHC.Parser                                          as GHC
import           GHC.Parser.Lexer
import           GHC.Types.SrcLoc
import           GHC.Utils.Outputable                                hiding
                                                                     (empty)
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config

main :: IO ()
main =
  getContents >>=
  printOutputable . showAstData BlankSrcSpan BlankEpAnnotations . parseModule

printOutputable :: Outputable a => a -> IO ()
printOutputable = putStrLn . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

parseModule :: String -> Located HsModule
parseModule code =
  case runParser parserOpts code GHC.parseModule of
    POk _ mod -> mod
    PFailed _ -> error "Failed to parse the code."

runParser :: ParserOpts -> String -> P a -> ParseResult a
runParser opts str parser = unP parser parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    b = stringToStringBuffer str
    parseState = initParserState opts b location

parserOpts :: ParserOpts
parserOpts = mkParserOpts empty empty False False True False
