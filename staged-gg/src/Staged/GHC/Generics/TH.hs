{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.GHC.Generics.TH (
    deriveGeneric,
    deriveGeneric1,
) where

import Control.Monad ((>=>), unless, when, forM)

-- template-haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- names
import Staged.GHC.Generics.TH.Names
import Staged.GHC.Generics.Internal (sapply)

-- th-abstraction
import Language.Haskell.TH.Datatype

-- from generic-deriving
import Generics.Deriving.TH.Internal
import Generics.Deriving.TH.Post4_9
import Generics.Deriving.TH
       (KindSigOptions, Options (..), RepOptions (..), defaultOptions)

-- th-lift
import Language.Haskell.TH.Lift ()

import qualified Data.Map as Map (empty, fromList)

deriveGeneric :: Name -> Q [Dec]
deriveGeneric n = do
    deriveInstCommon staged_genericTypeName  staged_repTypeName  Generic  staged_fromValName  staged_toValName defaultOptions n

deriveGeneric1 :: Name -> Q [Dec]
deriveGeneric1 n = do
    deriveInstCommon staged_generic1TypeName  staged_rep1TypeName  Generic1  staged_fromVal1Name  staged_toVal1Name defaultOptions n

deriveInstCommon :: Name
                 -> Name
                 -> GenericClass
                 -> Name
                 -> Name
                 -> Options
                 -> Name
                 -> Q [Dec]
deriveInstCommon genericName repName gClass fromName toName opts n = do
  i <- reifyDataInfo n

  let (name, instTys, cons, dv) = either error id i
      useKindSigs = kindSigOptions opts

  -- See Note [Forcing buildTypeInstance]
  !(origTy, origKind) <- buildTypeInstance gClass useKindSigs name instTys
  tyInsRHS <- if repOptions opts == InlineRep
                 then makeRepInline   gClass dv name instTys cons origTy
                 else makeRepTySynApp gClass dv name              origTy

  let origSigTy = if useKindSigs
                     then SigT origTy origKind
                     else origTy
  tyIns <- tySynInstDCompat repName
                            Nothing
                            [return origSigTy] (return tyInsRHS)

  let ecOptions = emptyCaseOptions opts
      mkBody maker = [clause [] (normalB $
        mkCaseExp gClass ecOptions name instTys cons maker) []]
      tcs = mkBody mkTo

      fcs' = do
        val <- newName "val"
        k   <- newName "_kont" -- avoids unused warning
        lamE [varP val, varP k] $
          [| unsafeCodeCoerce |] `appE` foldl appE [| caseE |]
            [ [| unTypeCode |] `appE` varE val
            , mkFrom (varE k) gClass ecOptions 1 1 name instTys cons
            ]

      fcs = [ clause [] (normalB fcs') []]

  fmap (:[]) $
    instanceD (cxt []) (conT genericName `appT` return origSigTy)
                         [return tyIns, funD fromName fcs, funD toName tcs]

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

-- For the given Types, deduces the instance type (and kind) to use for a
-- Generic(1) instance. Coming up with the instance type isn't as simple as
-- dropping the last types, as you need to be wary of kinds being instantiated
-- with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: GenericClass
                  -- ^ Generic or Generic1
                  -> KindSigOptions
                  -- ^ Whether or not to use explicit kind signatures in the instance type
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> Q (Type, Kind)
buildTypeInstance gClass useKindSigs tyConName varTysOrig = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - fromEnum gClass

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError tyConName

        -- Substitute kind * for any dropped kind variables
    let varTysExpSubst :: [Type]
-- See Note [Generic1 is polykinded in base-4.10]
#if MIN_VERSION_base(4,10,0)
        varTysExpSubst = varTysExp
#else
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati
#endif

    let remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

-- See Note [Generic1 is polykinded in base-4.10]
#if !(MIN_VERSION_base(4,10,0))
    -- If any of the dropped types were polykinded, ensure that there are of
    -- kind * after substituting * for the dropped kind variables. If not,
    -- throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError tyConName
#endif

        -- We now substitute all of the specialized-to-* kind variable names
        -- with *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
    let varTysOrigSubst :: [Type]
        varTysOrigSubst =
-- See Note [Generic1 is polykinded in base-4.10]
#if MIN_VERSION_base(4,10,0)
          id
#else
          map (substNamesWithKindStar droppedKindVarNames)
#endif
            $ varTysOrig

        remainingTysOrigSubst, droppedTysOrigSubst :: [Type]
        (remainingTysOrigSubst, droppedTysOrigSubst) =
            splitAt remainingLength varTysOrigSubst

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the useKindSigs check.
        remainingTysOrigSubst' =
          if useKindSigs
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceType :: Type
        instanceType = applyTyToTys (ConT tyConName) remainingTysOrigSubst'

        -- See Note [Kind signatures in derived instances]
        instanceKind :: Kind
        instanceKind = makeFunKind (map typeKind droppedTysOrigSubst) starK

    -- Ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceType, instanceKind)

makeRepInline :: GenericClass
              -> DatatypeVariant_
              -> Name
              -> [Type]
              -> [ConstructorInfo]
              -> Type
              -> Q Type
makeRepInline gClass dv name instTys cons ty = do
  let instVars = freeVariablesWellScoped [ty]
      (tySynVars, gk)  = genericKind gClass instTys

      typeSubst :: TypeSubst
      typeSubst = Map.fromList $
        zip (map tvName tySynVars)
            (map (VarT . tvName) instVars)

  repType gk dv name typeSubst cons

genRepName :: GenericClass -> DatatypeVariant_
           -> Name -> Name
genRepName gClass dv n
  = mkName
  . showsDatatypeVariant dv
  . (("Rep" ++ show (fromEnum gClass)) ++)
  . ((showNameQual n ++ "_") ++)
  . sanitizeName
  $ nameBase n

repType :: GenericKind
        -> DatatypeVariant_
        -> Name
        -> TypeSubst
        -> [ConstructorInfo]
        -> Q Type
repType gk dv dt typeSubst cs =
    conT d2TypeName `appT` mkMetaDataType dv dt `appT`
      foldBal sum' (conT v2TypeName) (map (repCon gk dv dt typeSubst) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT staged_sumTypeName `appT` a `appT` b

repCon :: GenericKind
       -> DatatypeVariant_
       -> Name
       -> TypeSubst
       -> ConstructorInfo
       -> Q Type
repCon gk dv dt typeSubst
  (ConstructorInfo { constructorName       = n
                   , constructorVars       = vars
                   , constructorContext    = ctxt
                   , constructorStrictness = bangs
                   , constructorFields     = ts
                   , constructorVariant    = cv
                   }) = do
  checkExistentialContext n vars ctxt
  let mbSelNames = case cv of
                     NormalConstructor          -> Nothing
                     InfixConstructor           -> Nothing
                     RecordConstructor selNames -> Just selNames
      isRecord   = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> False
                     RecordConstructor _ -> True
      isInfix    = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> True
                     RecordConstructor _ -> False
  ssis <- reifySelStrictInfo n bangs
  repConWith gk dv dt n typeSubst mbSelNames ssis ts isRecord isInfix

repConWith :: GenericKind
           -> DatatypeVariant_
           -> Name
           -> Name
           -> TypeSubst
           -> Maybe [Name]
           -> [SelStrictInfo]
           -> [Type]
           -> Bool
           -> Bool
           -> Q Type
repConWith gk dv dt n typeSubst mbSelNames ssis ts isRecord isInfix = do
    let structureType :: Q Type
        structureType = foldBal prodT (conT u2TypeName) f

        f :: [Q Type]
        f = case mbSelNames of
                 Just selNames -> zipWith3 (repField gk dv dt n typeSubst . Just)
                                           selNames ssis ts
                 Nothing       -> zipWith  (repField gk dv dt n typeSubst Nothing)
                                           ssis ts

    conT c2TypeName
      `appT` mkMetaConsType dv dt n isRecord isInfix
      `appT` structureType

prodT :: Q Type -> Q Type -> Q Type
prodT a b = conT staged_productTypeName `appT` a `appT` b

repField :: GenericKind
         -> DatatypeVariant_
         -> Name
         -> Name
         -> TypeSubst
         -> Maybe Name
         -> SelStrictInfo
         -> Type
         -> Q Type
repField gk dv dt ns typeSubst mbF ssi t =
           conT s2TypeName
    `appT` mkMetaSelType dv dt ns mbF ssi
    `appT` (repFieldArg gk False =<< resolveTypeSynonyms t'')
  where
    -- See Note [Generic1 is polykinded in base-4.10]
    t', t'' :: Type
    t' = case gk of
              Gen1 _ (Just _kvName) ->
#if MIN_VERSION_base(4,10,0)
                t
#else
                substNameWithKind _kvName starK t
#endif
              _ -> t
    t'' = applySubstitution typeSubst t'

repFieldArg :: GenericKind -> Bool -> Type -> Q Type
repFieldArg _ _ ForallT{} = rankNError
repFieldArg gk inPar (SigT t _) = repFieldArg gk inPar t
repFieldArg Gen0 _ t = boxT t
repFieldArg (Gen1 name _) _ (VarT t) | t == name = conT par2TypeName
repFieldArg gk@(Gen1 name _) inPar t = do
  let tyHead:tyArgs      = unapplyTy t
      numLastArgs        = min 1 $ length tyArgs
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs
      k2Type             = boxT t
      phiType            = return $ applyTyToTys tyHead lhsArgs

  let inspectTy :: Type -> Q Type
      inspectTy (VarT a)
        | a == name
        = if inPar
          then phiType
          else conT staged_appTypeName `appT` conT par2TypeName `appT` phiType
      inspectTy (SigT ty _) = inspectTy ty
      inspectTy beta
        | not (ground beta name)
        = conT staged_appTypeName
          `appT` (conT staged_appTypeName `appT` conT par2TypeName `appT` phiType)
          `appT` repFieldArg gk True beta
      inspectTy _ = k2Type

  itf <- isTyFamily tyHead
  if any (not . (`ground` name)) lhsArgs
       || any (not . (`ground` name)) tyArgs && itf
     then outOfPlaceTyVarError
     else case rhsArgs of
          []   -> k2Type
          ty:_ -> inspectTy ty

boxT :: Type -> Q Type
boxT ty = case unboxedRepNames ty of
    Just (boxTyName, _, _) -> conT boxTyName
    Nothing                -> conT k2TypeName `appT` return ty

unboxedRepNames :: Type -> Maybe (Name, Name, Name)
unboxedRepNames ty
  | ty == ConT addrHashTypeName   = Just (uAddrTypeName,   uAddrDataName,   uAddrHashValName)
  | ty == ConT charHashTypeName   = Just (uCharTypeName,   uCharDataName,   uCharHashValName)
  | ty == ConT doubleHashTypeName = Just (uDoubleTypeName, uDoubleDataName, uDoubleHashValName)
  | ty == ConT floatHashTypeName  = Just (uFloatTypeName,  uFloatDataName,  uFloatHashValName)
  | ty == ConT intHashTypeName    = Just (uIntTypeName,    uIntDataName,    uIntHashValName)
  | ty == ConT wordHashTypeName   = Just (uWordTypeName,   uWordDataName,   uWordHashValName)
  | otherwise                     = Nothing

makeRepTySynApp :: GenericClass -> DatatypeVariant_ -> Name
                -> Type -> Q Type
makeRepTySynApp gClass dv name ty =
  -- Here, we figure out the distinct type variables (in order from left-to-right)
  -- of the LHS of the Rep(1) instance. We call unKindedTV because the kind
  -- inferencer can figure out the kinds perfectly well, so we don't need to
  -- give anything here explicit kind signatures.
  let instTvbs = map unKindedTV $ freeVariablesWellScoped [ty]
  in return $ applyTyToTvbs (genRepName gClass dv name) instTvbs

mkCaseExp
  :: GenericClass -> EmptyCaseOptions -> Name -> [Type] -> [ConstructorInfo]
  -> (GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
                   -> [ConstructorInfo] -> Q Match)
  -> Q Exp
mkCaseExp gClass ecOptions dt instTys cs matchmaker = do
  val <- newName "val"
  lam1E (varP val) $ caseE (varE val) [matchmaker gClass ecOptions 1 1 dt instTys cs]

-- | 'True' if generated code for empty data types should use the @EmptyCase@
-- extension, 'False' otherwise. This has no effect on GHCs before 7.8, since
-- @EmptyCase@ is only available in 7.8 or later.
type EmptyCaseOptions = Bool

-------------------------------------------------------------------------------
-- mkTo
-------------------------------------------------------------------------------

mkTo :: GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
     -> [ConstructorInfo] -> Q Match
mkTo gClass ecOptions m i dt instTys cs = do
    y <- newName "y"
    match (conP m2DataName [varP y])
          (normalB $ caseE (varE y) cases)
          []
  where
    cases = case cs of
              [] -> errorTo ecOptions dt
              _  -> zipWith (toCon gk wrapP (length cs)) [1..] cs
    wrapP p = lrP i m p
    (_, gk) = genericKind gClass instTys


toCon :: GenericKind -> (Q Pat -> Q Pat) -> Int -> Int
      -> ConstructorInfo -> Q Match
toCon gk wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialContext cn vars ctxt
  fNames <- newNameList "f" $ length ts
  match (wrap $ lrP i m $ conP m2DataName
          [foldBal prod (conP u2DataName []) (zipWith (toField gk) fNames ts)])
        (normalB $ foldl
          (\f x -> [| sapply $f $x |])
          ([| unsafeCodeCoerce |] `appE` ([| conE |] `appE` lift cn))
          (zipWith (\nr -> resolveTypeSynonyms >=> toConUnwC gk nr)
          fNames ts)) []
  where prod x y = conP staged_productDataName [x,y]

toConUnwC :: GenericKind -> Name -> Type -> Q Exp
toConUnwC Gen0          nr _ = varE nr
toConUnwC (Gen1 name _) nr t = unwC t False name nr

toField :: GenericKind -> Name -> Type -> Q Pat
toField gk nr t = conP m2DataName [toFieldWrap gk nr t]

toFieldWrap :: GenericKind -> Name -> Type -> Q Pat
toFieldWrap Gen0   nr t = conP (boxRepName t) [varP nr]
toFieldWrap Gen1{} nr _ = varP nr

unwC :: Type -> Bool -> Name -> Name -> Q Exp
unwC (SigT t _) inPar name nr             = unwC t inPar name nr
unwC (VarT t)   inPar name nr | t == name = varE unPar2ValName `appE` varE nr
unwC t          inPar name nr
  | ground t name = varE (unboxRepName t) `appE` varE nr
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | a == name
            = if inPar
              then varE unAppValName `appE` varE nr
              else varE unPar2ValName `appE` (varE unAppValName `appE` varE nr)
          inspectTy beta
            = varE unPar2ValName `appE` (varE unAppValName `appE` unwC beta True name nr)

      itf <- isTyFamily tyHead
      if any (not . (`ground` name)) lhsArgs
           || any (not . (`ground` name)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> varE (unboxRepName t) `appE` varE nr
              ty:_ -> inspectTy ty

unboxRepName :: Type -> Name
unboxRepName = maybe unK2ValName trd3 . unboxedRepNames

boxRepName :: Type -> Name
boxRepName = maybe k2DataName snd3 . unboxedRepNames

-------------------------------------------------------------------------------
-- mkFrom
-------------------------------------------------------------------------------

mkFrom :: Q Exp
       -> GenericClass -> EmptyCaseOptions -> Int -> Int -> Name -> [Type]
       -> [ConstructorInfo] -> Q Exp
mkFrom kont gClass ecOptions m i dt instTys cs = do
    -- y <- newName "y"
    listE cases
    -- match (varP y)
    --       (normalB $ conE m2DataName `appE` caseE (varE y) cases)
    --       []
  where
    cases :: [ExpQ]
    cases = case cs of
              [] -> errorFrom ecOptions dt
              _  -> zipWith (fromCon kont gk wrapE (length cs)) [1..] cs
    wrapE e = lrE i m e
    (_, gk) = genericKind gClass instTys

errorFrom :: EmptyCaseOptions -> Name -> [ExpQ]
errorFrom useEmptyCase dt = []
{-
  | useEmptyCase && ghc7'8OrLater
  = []
  | otherwise
  = [do z <- newName "z"
        match
          (varP z)
          (normalB $
            appE (varE seqValName) (varE z) `appE`
            appE (varE errorValName)
                 (stringE $ "No generic representation for empty datatype "
                          ++ nameBase dt))
          []]
-}

fromCon :: Q Exp
        -> GenericKind -> (Q Exp -> Q Exp) -> Int -> Int
        -> ConstructorInfo -> Q Exp
fromCon kont gk wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialContext cn vars ctxt
  fNames <- newNameList' "f" $ length ts

  let fNameExps :: [ExpQ]
      fNameExps =
          [ [| unsafeCodeCoerce (varE $(varE fName)) |]
          | (fName, _) <- fNames
          ]

  let kontArg :: ExpQ
      kontArg = wrap $ lrE i m $ conE m2DataName `appE`
         foldBal prodE (conE u2DataName) (zipWith (fromField gk) fNameExps ts)

  -- we create a do block which makes new variables.
  let bindNewNames = [ bindS (varP v) [| newName $(stringE s) |] | (v, s) <- fNames ]

  doE $ bindNewNames ++

      -- match (conP cn (map varP fNames))
      --       (normalB $ wrap $ lrE i m $ conE m2DataName `appE`
      --         foldBal prodE (conE u2DataName) (zipWith (fromField gk) fNames ts)) []
      [ noBindS $ foldl appE [| match |]
          [ foldl appE [| conP |]
              [ lift cn
              , listE  [ [| varP |] `appE` varE fName | (fName, _) <- fNames ]
              ]
          , [| normalB (unTypeCode ($kont $(conE m2DataName `appE` kontArg))) |]
          , listE []
          ]
      ]

newNameList' :: String -> Int -> Q [(Name, String)]
newNameList' prefix n = forM [1..n] $ \i -> do
    let s = prefix ++ show i
    n <- newName s
    return (n, s)

prodE :: Q Exp -> Q Exp -> Q Exp
prodE x y = conE staged_productDataName `appE` x `appE` y

fromField :: GenericKind -> Q Exp -> Type -> Q Exp
fromField gk nr t = conE m2DataName `appE` (fromFieldWrap gk nr =<< resolveTypeSynonyms t)

fromFieldWrap :: GenericKind -> Q Exp -> Type -> Q Exp
fromFieldWrap _             _  ForallT{}  = rankNError
fromFieldWrap gk            nr (SigT t _) = fromFieldWrap gk nr t
fromFieldWrap Gen0          nr t          = conE (boxRepName t) `appE` nr
fromFieldWrap (Gen1 name _) nr t          = wC t name nr

wC :: Type -> Name -> Q Exp -> Q Exp
wC (VarT t) name nr | t == name = conE par2DataName `appE` nr
wC t        name nr
  | ground t name = conE (boxRepName t) `appE` nr
  | otherwise = do
      let tyHead:tyArgs      = unapplyTy t
          numLastArgs        = min 1 $ length tyArgs
          (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

          inspectTy :: Type -> Q Exp
          inspectTy ForallT{} = rankNError
          inspectTy (SigT ty _) = inspectTy ty
          inspectTy (VarT a)
            | a == name
            = conE appDataName `appE` (conE par2DataName `appE` nr)
          inspectTy beta =
              conE appDataName `appE` wC beta name nr

      itf <- isTyFamily tyHead
      if any (not . (`ground` name)) lhsArgs
           || any (not . (`ground` name)) tyArgs && itf
         then outOfPlaceTyVarError
         else case rhsArgs of
              []   -> conE (boxRepName t) `appE` nr
              ty:_ -> inspectTy ty

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

errorTo :: EmptyCaseOptions -> Name -> [Q Match]
errorTo useEmptyCase dt
  | useEmptyCase && ghc7'8OrLater
  = []
  | otherwise
  = [do z <- newName "z"
        match
          (varP z)
          (normalB $
            appE (varE seqValName) (varE z) `appE`
            appE (varE errorValName)
                 (stringE $ "No values for empty datatype " ++ nameBase dt))
          []]

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP i n p
  | n == 0       = fail "lrP: impossible"
  | n == 1       = p
  | i <= div n 2 = conP l2DataName [lrP i     (div n 2) p]
  | otherwise    = conP r2DataName [lrP (i-m) (n-m)     p]
                     where m = div n 2

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE i n e
  | n == 0       = fail "lrE: impossible"
  | n == 1       = e
  | i <= div n 2 = conE l2DataName `appE` lrE i     (div n 2) e
  | otherwise    = conE r2DataName `appE` lrE (i-m) (n-m)     e
                     where m = div n 2

ghc7'8OrLater :: Bool
#if __GLASGOW_HASKELL__ >= 708
ghc7'8OrLater = True
#else
ghc7'8OrLater = False
#endif

