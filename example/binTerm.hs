{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

import Data.Maybe (fromJust)

import Control.Isomorphism.Partial.Ext
  (Iso, IsoFunctor ((<$>)), subset)
import Text.Syntax.Poly
  ((<*>), (<|>), token,
   this, (*>), (<*), many, optional, (<$?>), format)
import Text.Syntax.Poly.Type (SyntaxT)

import Text.Syntax.Check.Parsec (printParseIsoDefault)
import Text.Syntax.Printer.List (runPrinter)

import qualified Types as T


type Exp = T.ExpT Char
type BinSyntax a = SyntaxT Char a

justC :: Char -> BinSyntax ()
justC =  this

lParen :: BinSyntax ()
lParen =  justC '('

rParen :: BinSyntax ()
rParen =  justC ')'

alpha :: BinSyntax Char
alpha =  subset (`elem` ['a' .. 'z']) <$> token

var :: BinSyntax Exp
var = T.var <$> alpha

indentUnit :: String
indentUnit =  replicate 4 ' '

indentWith :: [Char] -> BinSyntax ()
indentWith =  format

indent :: BinSyntax ()
indent =  indentWith indentUnit

zeroIndent :: BinSyntax ()
zeroIndent =  indentWith []

newline :: BinSyntax ()
newline =  format "\n"

(*>|) :: BinSyntax () -> BinSyntax () -> BinSyntax ()
indent' *>| syn =  indent' *> syn <* newline

-- When printing parened expression,
-- must check formula without paren or not first.
-- Check var or not first below.
uni :: BinSyntax Exp
uni =  var
  <|>  lParen *> expr <* rParen

bin :: Iso (a, a) a -> BinSyntax a -> Char -> BinSyntax a
bin cons upper op = cons <$?> upper <*> optional (justC op *> upper)

power :: BinSyntax Exp
power =  bin T.pow  uni   '^'

flac  :: BinSyntax Exp
flac  =  bin T.flac power '/'

mult  :: BinSyntax Exp
mult  =  bin T.mult flac  '*'

minus :: BinSyntax Exp
minus =  bin T.sub  mult  '-'

plus  :: BinSyntax Exp
plus  =  bin T.plus minus '+'

expr :: BinSyntax Exp
expr =  plus

semi :: BinSyntax ()
semi =  justC ';'

lBrace :: BinSyntax ()
lBrace =  justC '{'

rBrace :: BinSyntax ()
rBrace =  justC '}'

lBracket :: BinSyntax ()
lBracket =  justC '['

rBracket :: BinSyntax ()
rBracket =  justC ']'

type Elem  = T.ElemT  Char
type Block = T.BlockT Char

elem' :: BinSyntax () -> BinSyntax Elem
elem' indent' = T.blockE   <$> block indent' lBrace   rBrace <|>
                T.bracketE <$> block indent' lBracket rBracket  <|>
                T.expE     <$> indent' *> expr <* semi <* newline

elems :: BinSyntax () -> BinSyntax [Elem]
elems indent' =  many (elem' indent')

block :: BinSyntax () -> BinSyntax () -> BinSyntax () -> BinSyntax Block
block indent' l r = T.block <$>
                    (indent' *>| l) *>
                    elems (indent *> indent')
                    <* (indent' *>| r)

type Program = T.ProgramT Char

program :: BinSyntax Program
program =  T.program <$> elems zeroIndent

ex0 =  "a"
ex1 =  "a^(b^c)"
ex2 =  "a/b^c"
ex3 =  "a/b^c*d"
ex4 =  "a/b-c*d^e"

ex01 = "(a+(b*c))"
ex02 = "((a+b)*(z+x))"
ex03 = "((a+t)*((b+(a+c))^(c+d)))"
ex04 = "((a+t)*(b+(a+c)))^(c+d)"

ex0, ex1, ex2, ex3, ex4, ex01, ex02, ex03, ex04 :: String

prog0 :: String
prog0 =  "{(a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}a/b-c*d^e;}"

prog1 :: String
prog1 =  "{(a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));]a/b-c*d^e;}"

prog2 :: String
prog2 =  "{(a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));](a+(b*c));((a+b)*(z+x));{((a+t)*((b+(a+c))^(c+d)));((a+t)*(b+(a+c)))^(c+d);}[(a+(b*c));((a+b)*(z+x));]a/b-c*d^e;}"

prog3 :: String
prog3 =  "{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));{(a+(b*c));}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

allExpTests :: [String]
allExpTests =  [ex0, ex1, ex2, ex3, ex4,
                ex01, ex02, ex03, ex04]

allProgTests :: [String]
allProgTests =  [prog0, prog1, prog2, prog3]


exprPPIso :: [Char] -> Either String Exp
exprPPIso =  printParseIsoDefault expr

programPPIso :: [Char] -> Either String Program
programPPIso =  printParseIsoDefault program


showResult :: BinSyntax a -> Either String a -> String
showResult syn =  d where
  d (Right r) = "Good isomorphism syntax:\n" ++
                fromJust (runPrinter syn r) ++ "\n"
  d (Left  e) = e

main :: IO ()
main = mapM_ putStrLn
       (map (showResult expr    . exprPPIso   ) allExpTests ++
        map (showResult program . programPPIso) allProgTests)
