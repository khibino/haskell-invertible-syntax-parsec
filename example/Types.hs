{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

data ExpT t  = Var t
             | Pow  (ExpT t) (ExpT t)
             | Flac (ExpT t) (ExpT t)
             | Mult (ExpT t) (ExpT t)
             | Sub  (ExpT t) (ExpT t)
             | Plus (ExpT t) (ExpT t)
             deriving (Show, Eq)

data ElemT t = ExpE (ExpT t)
             | BlockE (BlockT t)
             | BracketE (BlockT t)
             deriving (Show, Eq)

type Elems t = [ElemT t]

data BlockT   t = Block (Elems t)  deriving (Show, Eq)
data ProgramT t = Program (Elems t)  deriving (Show, Eq)


$(defineIsomorphisms ''ExpT)
$(defineIsomorphisms ''ElemT)
$(defineIsomorphisms ''BlockT)
$(defineIsomorphisms ''ProgramT)
