{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Syntax.Parser.Parsec (
  -- * Run Syntax as Parsec
  runPolyParser
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative(try, (<|>)),
   StreamSyntax (..), Syntax(..), RunParser, RunParserM)

import Text.Parsec (ParsecT, Stream, ParseError)
import qualified Text.Parsec as P

-- ParsecT s u m is instance of MonadPlus.
-- Instance definitions of IsoFunctor, ProductFunctor, IsoAlternative and AbstractSyntax
-- is derived from MonadPlus by definitions in Text.Syntax.Poly.Instances

instance TryAlternative (ParsecT s u m)
  where try = P.try
        p <|> q = try p <||> q

instance (Eq t, Show t, Stream [t] m t) =>
         StreamSyntax [t] (ParsecT [t] u m)
  where string tks = P.tokens show (\pos _ -> pos) tks >> return ()

instance (StreamSyntax [t] (ParsecT [t] u m), Stream [t] m t, Show t) =>
         Syntax t [t] (ParsecT [t] u m)
  where token = P.anyToken

runPolyParser :: (Eq tok, Show tok) =>
                 String -> RunParser tok [tok] a ParseError
runPolyParser srcName parser =
  P.parse parser srcName

runPolyParserA :: (Eq tok, Show tok, Functor m, Monad m) =>
                  String -> RunParserM m tok [tok] a ParseError
runPolyParserA srcName parser =
  P.runParserT parser () srcName
