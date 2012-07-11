{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Text.Syntax.Check.Parsec (
  -- * Handy isomorphism check functions.
  printParseIsoDefault',  printParseIsoDefault
  ) where

import Text.Syntax.Poly (SyntaxT)

import Text.Syntax.Printer.List (runPolyPrinter)
import Text.Syntax.Check.Prim (printParseIso', printParseIso)

import Text.Syntax.Parser.Parsec (runPolyParser)

printParseIsoDefault' :: (Eq tok, Eq a, Show tok) => SyntaxT tok a -> a -> Either String a
printParseIsoDefault' =  printParseIso' runPolyPrinter (runPolyParser "<check>")

printParseIsoDefault :: (Eq tok, Eq a, Show tok) => SyntaxT tok a -> [tok] -> Either String a
printParseIsoDefault =  printParseIso runPolyPrinter (runPolyParser "<check>")
