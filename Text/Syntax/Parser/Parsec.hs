{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Syntax.Parser.Parsec (
  -- * Run Syntax as Parsec
  runAsParsec, runAsParsecM
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative(try, (<|>)),
   Syntax(..), RunAsParser, RunAsParserM)

import Text.Parsec (ParsecT, Stream, ParseError)
import qualified Text.Parsec as P

-- ParsecT s u m is instance of MonadPlus.
-- Instance definitions of IsoFunctor, ProductFunctor, IsoAlternative and AbstractSyntax
-- is derived from MonadPlus by definitions in Text.Syntax.Poly.Instances

instance TryAlternative (ParsecT s u m)
  where try = P.try
        p <|> q = try p <||> q

instance (Stream [t] m t, Show t) =>
         Syntax t (ParsecT [t] u m)
  where token = P.anyToken

runAsParsec :: (Eq tok, Show tok) =>
                 String -> RunAsParser tok [tok] a ParseError
runAsParsec srcName parser =
  P.parse parser srcName

runAsParsecM :: (Eq tok, Show tok, Functor m, Monad m) =>
                  String -> RunAsParserM m tok [tok] a ParseError
runAsParsecM srcName parser =
  P.runParserT parser () srcName
