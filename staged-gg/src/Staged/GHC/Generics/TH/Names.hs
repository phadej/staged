{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.GHC.Generics.TH.Names where

import Staged.GHC.Generics.Types

import Language.Haskell.TH.Syntax (Name)

staged_genericTypeName :: Name
staged_genericTypeName = ''Generic

staged_repTypeName :: Name
staged_repTypeName = ''Rep

staged_generic1TypeName :: Name
staged_generic1TypeName = ''Generic1

staged_rep1TypeName :: Name
staged_rep1TypeName = ''Rep1

c2TypeName :: Name
c2TypeName = ''C2

d2TypeName :: Name
d2TypeName = ''D2

s2TypeName :: Name
s2TypeName = ''S2

staged_productTypeName :: Name
staged_productTypeName = ''(:**:)

staged_sumTypeName :: Name
staged_sumTypeName = ''(:++:)

staged_appTypeName :: Name
staged_appTypeName = ''(:@@:)

appDataName :: Name
appDataName = 'App2

unAppValName :: Name
unAppValName = 'unApp2

u2TypeName :: Name
u2TypeName = ''U2

v2TypeName :: Name
v2TypeName = ''V2

par2TypeName :: Name
par2TypeName = ''Par2

par2DataName :: Name
par2DataName = 'Par2

unPar2ValName :: Name
unPar2ValName = 'unPar2

k2TypeName :: Name
k2TypeName = ''K2

staged_toValName :: Name
staged_toValName = 'to

staged_fromValName :: Name
staged_fromValName = 'from

staged_toVal1Name :: Name
staged_toVal1Name = 'to1

staged_fromVal1Name :: Name
staged_fromVal1Name = 'from1

k2DataName :: Name
k2DataName = 'K2

unK2ValName :: Name
unK2ValName = 'unK2

l2DataName :: Name
l2DataName = 'L2

m2DataName :: Name
m2DataName = 'M2

staged_productDataName :: Name
staged_productDataName = '(:**:)

r2DataName :: Name
r2DataName = 'R2

u2DataName :: Name
u2DataName = 'U2
