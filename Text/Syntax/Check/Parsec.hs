{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Text.Syntax.Check.Parsec (
  -- * Handy isomorphism check functions.
  printParseIso0Default,  printParseIsoDefault
  ) where

import Text.Syntax.Poly (SyntaxT)

import Text.Syntax.Printer.List (runAsPrinter)
import Text.Syntax.Check.Prim (printParseIso0, printParseIso)

import Text.Syntax.Parser.Parsec (runAsParsec)

printParseIso0Default :: (Eq tok, Eq a, Show tok) => SyntaxT tok a -> a -> Either String a
printParseIso0Default =  printParseIso0 runAsPrinter (runAsParsec "<check>")

printParseIsoDefault :: (Eq tok, Eq a, Show tok) => SyntaxT tok a -> [tok] -> Either String a
printParseIsoDefault =  printParseIso runAsPrinter (runAsParsec "<check>")
