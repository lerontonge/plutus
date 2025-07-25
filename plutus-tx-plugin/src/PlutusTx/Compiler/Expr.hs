-- editorconfig-checker-disable-file
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Functions for compiling GHC Core expressions into Plutus Core terms.
module PlutusTx.Compiler.Expr (compileExpr, compileExprWithDefs, compileDataConRef) where

import GHC.Builtin.Names qualified as GHC
import GHC.Builtin.Types.Prim qualified as GHC
import GHC.ByteCode.Types qualified as GHC
import GHC.Core qualified as GHC
import GHC.Core.Class qualified as GHC
import GHC.Core.Multiplicity qualified as GHC
import GHC.Core.TyCo.Rep qualified as GHC
import GHC.Num.Integer qualified
import GHC.Plugins qualified as GHC
import GHC.Tc.Utils.TcType qualified as GHC
import GHC.Types.CostCentre qualified as GHC
import GHC.Types.Id.Make qualified as GHC
import GHC.Types.Tickish qualified as GHC

import PlutusTx.AsData.Internal qualified
import PlutusTx.Bool qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Compiler.Binders
import PlutusTx.Compiler.Builtins
import PlutusTx.Compiler.Error
import PlutusTx.Compiler.Laziness
import PlutusTx.Compiler.Names
import PlutusTx.Compiler.Trace
import PlutusTx.Compiler.Type
import PlutusTx.Compiler.Types
import PlutusTx.Compiler.Utils
import PlutusTx.Coverage
import PlutusTx.Function qualified
import PlutusTx.Optimize.Inline qualified
import PlutusTx.PIRTypes
import PlutusTx.PLCTypes (PLCType, PLCVar)

-- I feel like we shouldn't need this, we only need it to spot the special String type, which is annoying
import PlutusTx.Builtins.HasOpaque qualified as Builtins
import PlutusTx.Trace

import PlutusIR qualified as PIR
import PlutusIR.Analysis.Builtins
import PlutusIR.Compiler.Definitions qualified as PIR
import PlutusIR.Compiler.Names (safeFreshName)
import PlutusIR.Core.Type (Term (..))
import PlutusIR.MkPir qualified as PIR
import PlutusIR.Purity qualified as PIR

import PlutusCore qualified as PLC
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import PlutusCore.StdLib.Data.Function qualified
import PlutusCore.Subst qualified as PLC

import Control.Exception (displayException)
import Control.Lens hiding (index, strict, transform)
import Control.Monad
import Control.Monad.Reader (ask, asks, local)
import Data.Array qualified as Array
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.Generics.Uniplate.Data (transform, universeBi)
import Data.List (elemIndex, isPrefixOf, isSuffixOf)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Traversable (for)
import Data.Tuple.Extra
import Data.Word (Word8)

{- Note [System FC and System FW]
Haskell uses system FC, which includes type equalities and coercions.

PLC does *not* have coercions in particular. However, PLC also does not have a nominal
type system - everything is constructed via operators on base types, so we have no
need for coercions to convert between newtypes and datatypes.
-}

-- Literals and primitives

{- Note [Literals]
GHC's literals and primitives are a bit of a pain, since they not only have a Literal
containing the actual data, but are wrapped in special functions (often ending in the magic #).

This is a pain to recognize.

Fortunately, in practice the only kind of literals we need to deal with directly are integer literals.
String literals are handled specially, see Note [String literals].
-}

{- Note [unpackFoldrCString#]
This function is introduced by rewrite rules, and usually eliminated by them
in concert with `build`.

However, since we often mark things as INLINABLE, we get pre-optimization Core
where only the first transformation has fired. So we need to do something with
the function.

- We can't easily turn it into a normal fold expression, since we'd need to make
  a lambda and we're not in 'CoreM' so we can't make fresh names.

- We can't easily translate it to a builtin, since we don't support higher-order
  functions.

So we use a horrible hack and match on `build . unpackFoldrCString#` to "undo"
the original rewrite rule.

Moreover, in some cases GHC generates such expressions where the first literal
character is "un-consed" from its tail, for example:

    GHC.Types.:
      @GHC.Types.Char
      (GHC.Types.C# 'f'#)
      (GHC.Base.build
        @GHC.Types.Char
        (\@b -> GHC.CString.unpackFoldrCString# @b "0d1"#)
      )

Then we re-do the cons after un-doing the original rewrite rule.
-}

compileLiteral
  :: (CompilingDefault uni fun m ann)
  => GHC.Literal
  -> m (PIRTerm uni fun)
compileLiteral = \case
  -- Just accept any kind of number literal, we'll complain about types we don't support elsewhere
  (GHC.LitNumber _ i) -> pure $ PIR.embedTerm $ PLC.mkConstant annMayInline i
  GHC.LitString _ -> throwPlain $ UnsupportedError "Literal string (maybe you need to use OverloadedStrings)"
  GHC.LitChar _ -> throwPlain $ UnsupportedError "Literal char"
  GHC.LitFloat _ -> throwPlain $ UnsupportedError "Literal float"
  GHC.LitDouble _ -> throwPlain $ UnsupportedError "Literal double"
  GHC.LitLabel{} -> throwPlain $ UnsupportedError "Literal label"
  GHC.LitNullAddr -> throwPlain $ UnsupportedError "Literal null"
  GHC.LitRubbish{} -> throwPlain $ UnsupportedError "Literal rubbish"

{-| Get the bytestring content of a string expression, if possible.
Follows (Haskell) variable references!
-}
tryStringLiteralAsBytes :: GHC.CoreExpr -> Maybe BS.ByteString
tryStringLiteralAsBytes coreExpr = case coreExpr of
  GHC.Lit (GHC.LitString bytes) ->
    Just bytes
  GHC.Var isUnpackCString `GHC.App` GHC.Lit (GHC.LitString bytes)
    | GHC.getName isUnpackCString == GHC.unpackCStringName ->
        Just bytes
  GHC.Var isUnpackCStringUtf8 `GHC.App` GHC.Lit (GHC.LitString bytes)
    | GHC.getName isUnpackCStringUtf8 == GHC.unpackCStringUtf8Name ->
        Just bytes
  {- See Note [unpackFoldrCString#]

  Example GHC Core expr this pattern matches:
    GHC.Base.build
      @GHC.Types.Char
      (\@b -> GHC.CString.unpackFoldrCString# @b "0d1"#)
  -}
  GHC.Var build
    `GHC.App` GHC.Type (GHC.TyConApp charTyCon _kindOrType)
    `GHC.App` GHC.Lam _ (GHC.Var unpack `GHC.App` _ `GHC.App` expr)
      | GHC.getName build == GHC.buildName
      , GHC.getName unpack == GHC.unpackCStringFoldrName
      , GHC.getName charTyCon == GHC.charTyConName ->
          tryStringLiteralAsBytes expr
  {-
  Example GHC Core expr this pattern matches:
    GHC.Types.: @GHC.Types.Char (GHC.Types.C# 'f'#) expr
  -}
  GHC.Var consId
    `GHC.App` GHC.Type (GHC.TyConApp charTyCon _kindOrType)
    `GHC.App` (GHC.Var cSharp `GHC.App` GHC.Lit (GHC.LitChar c))
    `GHC.App` expr
      | GHC.getName charTyCon == GHC.charTyConName
      , Just consDataCon <- GHC.isDataConId_maybe consId
      , GHC.consDataCon == consDataCon
      , Just charDataCon <- GHC.isDataConId_maybe cSharp
      , GHC.charDataCon == charDataCon ->
          BSC.cons c <$> tryStringLiteralAsBytes expr
  -- GHC helpfully generates an empty list for the empty string literal instead
  -- of a 'LitString'
  GHC.Var nil `GHC.App` GHC.Type (GHC.tyConAppTyCon_maybe -> Just tc)
    | nil == GHC.dataConWorkId GHC.nilDataCon
    , GHC.getName tc == GHC.charTyConName ->
        Just mempty
  -- Chase variable references! GHC likes to lift string constants to variables,
  -- that is not good for us!
  GHC.Var (GHC.maybeUnfoldingTemplate . GHC.realIdUnfolding -> Just unfolding) ->
    tryStringLiteralAsBytes unfolding
  _ -> Nothing

{-| Given a GHC Core expression representing a string literal
extracts a ByteString from it.
-}
stringLiteralAsBytes
  :: (Compiling uni fun m ann)
  => GHC.Name
  -- ^ is used for error reporting.
  -> GHC.CoreExpr
  -- ^ The expression to extract the ByteString from.
  -> m BS.ByteString
stringLiteralAsBytes name coreExpr =
  case tryStringLiteralAsBytes coreExpr of
    Just bytes -> pure bytes
    Nothing ->
      throwSd CompilationError $
        "Use of fromString @"
          GHC.<+> GHC.ppr name
          GHC.<+> "with inscrutable content: "
          GHC.<+> GHC.ppr coreExpr

{-| Given a GHC Core expression representing a string literal
extracts UTF-8 encoded ByteString from it and decodes it as Text
-}
stringLiteralAsText :: (Compiling uni fun m ann) => GHC.Name -> GHC.CoreExpr -> m T.Text
stringLiteralAsText name coreExpr = do
  bytes <- stringLiteralAsBytes name coreExpr
  case TE.decodeUtf8' bytes of
    Right txt -> pure txt
    Left err ->
      throwSd CompilationError $
        "Invalid UTF-8 in string literal:"
          GHC.<+> GHC.text (displayException err)

{-| Tries to recover original bytes from a UTF-8 encoded bytestring literal.

This isn't a full UTF-8 decoder: it only decodes the subset of UTF-8 that
is expected to be found in bytestring literals: 0x00 - 0xFF

If 'ByteString' contains a codepoint that is not in this range, the function will throw an error.
-}
utf8CodePointsAsBytes :: (Compiling uni fun m ann) => BS.ByteString -> m BS.ByteString
utf8CodePointsAsBytes bs =
  case tryUtf8CodePointsAsBytes bs of
    Just bytes -> pure bytes
    Nothing ->
      throwPlain . CompilationError $
        "ByteString literal is expected to contain only codepoints in the range 0 - 255 (0x00 - 0xFF)"

tryUtf8CodePointsAsBytes :: BS.ByteString -> Maybe BS.ByteString
tryUtf8CodePointsAsBytes = fmap BS.pack . gracefullyDecodeUtf8Bytes . BS.unpack
 where
  {-
  Why not use 'Data.Text.Encoding'?
  1. Some bytes never appear in UTF-8 encoded text (0xC0, 0xC1, 0xF5-0xFF).
  2. GHC Core could contain such bytes in bytestring literals,
     e.g. "\0\1" is stored as "\192\128\SOH".
  3. The UTF-8 parser from 'Data.Text.Encoding' chokes on these bytes:
      ghci> TE.decodeUtf8 "\192\128\SOH"
      *** Exception: Cannot decode byte '\xc0': Data.Text.Encoding: Invalid UTF-8 stream
  4. In the custom parsing logic below we can handle these bytes:
  -}
  gracefullyDecodeUtf8Bytes :: [Word8] -> Maybe [Word8]
  gracefullyDecodeUtf8Bytes = \case
    [] -> Just []
    192 : 128 : rest -> (0x00 :) <$> gracefullyDecodeUtf8Bytes rest
    194 : b : rest | b > 127 && b < 192 -> (b :) <$> gracefullyDecodeUtf8Bytes rest
    195 : b : rest | b > 127 && b < 192 -> ((b + 64) :) <$> gracefullyDecodeUtf8Bytes rest
    b : rest | b > 0 && b < 128 -> (b :) <$> gracefullyDecodeUtf8Bytes rest
    _ -> Nothing

{-| Strip off irrelevant things when we're trying to match a particular pattern in the code. Mostly ticks.
We only need to do this as part of a complex pattern match: if we're just compiling the expression
in question we will strip this off anyway.
-}
strip :: GHC.CoreExpr -> GHC.CoreExpr
strip = \case
  GHC.Var n `GHC.App` GHC.Type _ `GHC.App` expr | GHC.getName n == GHC.noinlineIdName -> strip expr
  GHC.Tick _ expr -> strip expr
  expr -> expr

-- | Convert a reference to a data constructor, i.e. a call to it.
compileDataConRef :: (CompilingDefault uni fun m ann) => GHC.DataCon -> m (PIRTerm uni fun)
compileDataConRef dc = do
  dcs <- getDataCons tc
  constrs <- getConstructors tc

  -- TODO: this is inelegant
  index <- case elemIndex dc dcs of
    Just i -> pure i
    Nothing ->
      throwPlain $
        CompilationError "Data constructor not in the type constructor's list of constructors"

  pure $ constrs !! index
 where
  tc = GHC.dataConTyCon dc

-- | Make alternatives with non-delayed and delayed bodies for a given 'CoreAlt'.
compileAlt
  :: (CompilingDefault uni fun m ann)
  => GHC.CoreAlt
  -- ^ The 'CoreAlt' representing the branch itself.
  -> [GHC.Type]
  -- ^ The instantiated type arguments for the data constructor.
  -> PIRTerm uni fun
  -> m (PIRTerm uni fun, PIRTerm uni fun)
  -- ^ Non-delayed and delayed
compileAlt (GHC.Alt alt vars body) instArgTys defaultBody =
  traceCompilation 3 ("Creating alternative:" GHC.<+> GHC.ppr alt) $ case alt of
    GHC.LitAlt _ -> throwPlain $ UnsupportedError "Literal case"
    -- We just package it up as a lambda bringing all the
    -- vars into scope whose body is the body of the case alternative.
    -- See Note [Iterated abstraction and application]
    -- See Note [Case expressions and laziness]
    GHC.DataAlt _ -> withVarsScoped ((,Nothing) <$> vars) $ \vars' -> do
      b <- compileExpr body
      delayed <- delay b
      return (PLC.mkIterLamAbs vars' b, PLC.mkIterLamAbs vars' delayed)
    GHC.DEFAULT -> do
      -- ignore the body in the alt, because we've got a pre-compiled one
      let compiledBody = defaultBody
      nonDelayed <- wrapDefaultAlt compiledBody
      delayed <- delay compiledBody >>= wrapDefaultAlt
      return (nonDelayed, delayed)
 where
  wrapDefaultAlt :: (CompilingDefault uni fun m ann) => PIRTerm uni fun -> m (PIRTerm uni fun)
  wrapDefaultAlt body' = do
    -- need to consume the args
    argTypes <- mapM compileTypeNorm instArgTys
    argNames <-
      forM [0 .. (length argTypes - 1)] (\i -> safeFreshName $ "default_arg" <> (T.pack $ show i))
    pure $ PIR.mkIterLamAbs (zipWith (PIR.VarDecl annMayInline) argNames argTypes) body'

-- See Note [GHC runtime errors]
isErrorId :: GHC.Id -> Bool
isErrorId ghcId = ghcId `elem` GHC.errorIds

-- See Note [Uses of Eq]
isProbablyBytestringEq :: GHC.Id -> Bool
isProbablyBytestringEq (GHC.getName -> n)
  | Just m <- GHC.nameModule_maybe n
  , GHC.moduleNameString (GHC.moduleName m) == "Data.ByteString.Internal"
      || GHC.moduleNameString (GHC.moduleName m) == "Data.ByteString.Lazy.Internal"
  , GHC.occNameString (GHC.nameOccName n) == "eq" =
      True
isProbablyBytestringEq _ = False

isProbablyIntegerEq :: GHC.Id -> Bool
isProbablyIntegerEq (GHC.getName -> n)
  | Just m <- GHC.nameModule_maybe n
  , GHC.moduleNameString (GHC.moduleName m) == "GHC.Num.Integer"
  , GHC.occNameString (GHC.nameOccName n) == "integerEq" =
      True
isProbablyIntegerEq _ = False

{-| Check for literal ranges like [1..9] and [1, 5..101].  This will also
return `True` if there's an explicit use of `enumFromTo` or similar.
-}
isProbablyBoundedRange :: GHC.Id -> Bool
isProbablyBoundedRange (GHC.getName -> n)
  | Just m <- GHC.nameModule_maybe n
  , GHC.moduleNameString (GHC.moduleName m) == "GHC.Enum" =
      ( "$fEnum" `isPrefixOf` methodName
          && ( "_$cenumFromTo" `isSuffixOf` methodName -- [1..100]
                 || "_$cenumFromThenTo" `isSuffixOf` methodName -- [1,3..100]
             )
      )
        || "enumDeltaToInteger" `isPrefixOf` methodName
 where
  -- \^ These are introduced by inlining for Integer ranges in
  -- GHC.Enum. This also happens for Char, Word, and Int, but those types
  -- aren't supported in Plutus Core.
  methodName = GHC.occNameString (GHC.nameOccName n)
isProbablyBoundedRange _ = False

{-| Check for literal ranges like [1..] and [1, 5..].  This will also return
`True` if there's an explicit use of `enumFrom` or similar.
-}
isProbablyUnboundedRange :: GHC.Id -> Bool
isProbablyUnboundedRange (GHC.getName -> n)
  | Just m <- GHC.nameModule_maybe n
  , GHC.moduleNameString (GHC.moduleName m) == "GHC.Enum" =
      ( "$fEnum" `isPrefixOf` methodName
          && ( "_$cenumFrom" `isSuffixOf` methodName -- [1..]
                 || "_$cenumFromThen" `isSuffixOf` methodName -- [1,3..]
             )
      )
        || "enumDeltaInteger" `isPrefixOf` methodName -- Introduced by inlining
 where
  methodName = GHC.occNameString (GHC.nameOccName n)
isProbablyUnboundedRange _ = False

{- Note [GHC runtime errors]
GHC has a number of runtime errors for things like pattern matching failures and so on.

We just translate these directly into calls to error, throwing away any other information.
-}

{- Note [Uses of Eq]
Eq can pop up in some annoying places:
- Literal patterns can introduce guards that use == from Eq
- Users can just plain use it instead of our Eq

This typically then leads to things we can't compile.

So, we can try and give an error when people do this. The obvious thing to do is to give an
error if we see a method of Eq. However, this doesn't work since the methods often get
inlined before we see them, either by the simplifier pass we run on our own module, or
because the simplifier does at least gentle inlining on unfoldings from other modules
before we see them.

So we have a few special cases in addition to catch things that look like inlined Integer or
ByteString equality, since those are especially likely to come up.
-}

{- Note [At patterns]
GHC handles @-patterns by adding a variable to each case expression representing the scrutinee
of the expression.

We handle this by simply let-binding that variable outside our generated case.

However, there is a subtlety: we'd like this binding to be removed by the dead-binding removal pass in PIR,
but only where we don't absolutely need it to be sure the scrutinee is evaluated. Fortunately, provided
we do a pattern match at all we will evaluate the scrutinee, since we do pattern matching by applying the scrutinee.

So the only case where we *need* to keep the binding in place is the case described in Note [Evaluation-only cases].
In this case we make a strict binding, in all others we make a non-strict binding.
-}

{- Note [Evaluation-only cases]
GHC sometimes generates case expressions where there is only a single alternative, and where none
of the variables bound by the alternative are live (see Note [Occurrence analysis] for how we tell
that this is the case).

What this amounts to is ensuring the expression is evaluated - hence one place this appears is bang
patterns.

It can do this even if the argument is a type variable (i.e. not known to be a datatype) by producing
a default-only case expression! Also, this can happen to our opaque builtin wrapper types in the
presence of e.g. bang patterns.

We can't actually compile this as a pattern match, since we need to know the actual type to do that,
(or in the case of builtin wrapper types, they're supposed to be opaque!).
But in the case where there is only one alternative with no live variables, we don't *need* to, because it
doesn't actually *do* anything with the contents of the datatype. So we can just compile this by returning
the body of the alternative wrapped in a strict let which binds the scrutinee. That achieves the
same thing as GHC wants (since GHC does expect the scrutinee to be in scope!).
-}

{- Note [Coercions and newtypes]
GHC is keen to put coercions in, they're usually great for it. However, this is a pain for us, since
you can have all kinds of fancy coercions, like coercions between functions where some of the arguments
are newtypes. We don't need to support all the stuff you can do with coercions, but we do want to
support newtypes.

A previous approach was to inspect coercions to try and work out if they were coercions between a newtype
and its underlying type, and if so manually construct/deconstruct it. This had a number of disadvantages.
- It only worked on very specific cases (e.g. if the simplifier gets loose it can make more complicated
  coercions that we can't obviously deconstruct without much more work)
- It wasn't future-proof. It's likely that GHC will move in the direction of getting rid of the structure
  of coercions (see https://gitlab.haskell.org//ghc/ghc/issues/8095#note_108189), so this approach might
  well stop working in the future.

So we would like to "believe" coercions, for at least some cases. We can
do this by always treating a newtype as it's underlying type. Except - this doesn't work for recursive
newtypes (we loop!). GHC doesn't have this problem because it treats the underlying type and the
newtype as separate types that happen to have the same representation. We don't have a separate representation
so we don't have that option.

So for the moment we:
- Treat newtypes as their underlying type.
- Blackhole newtypes when we start converting them so we can bail if they're recursive.
- Always believe coercions (i.e. just treat casts as the identity).

The final point could get us into trouble with fancier uses of coercions (since we will just accept them),
but those should fail when we typecheck the PLC. And we explicitly say we don't support such things.
-}

{- Note [Unfoldings]
GHC stores the current RHS of bindings in "unfoldings". These are used for inlining, but
also generally provide the compiler's view of the RHS of a binding. They are usually available
for other modules in the same package, and can be available cross-package if GHC decides it's
a good idea or if the binding is marked INLINABLE (or if you use `-fexpose-all-unfoldings`).

We use unfoldings to get the definitions of non-locally bound names. We then hoist these into
definitions using PIR's support for definitions. This allows a relatively direct form of code
reuse - provided that the code you are reusing has unfoldings! In practice this means you may
need to scatter some INLINABLE pragmas around, but we may be able to improve this in future,
see e.g. https://gitlab.haskell.org/ghc/ghc/issues/10871.

(Since unfoldings are updated as the compiler progresses, unfoldings for bindings in other
modules are typically fully-optimized. The exception is the unfoldings for INLINABLE bindings,
which get the *pre* optimization RHS. This is so that rewrite rules can fire. In practice, this
means that we need to be okay getting either.)
-}

{- Note [Non-strict let-bindings]
Haskell is a non-strict language, PLC is a strict language. One place that can show up is in let-bindings.
In particular, a let-binding where the RHS is not value may behave differently.
e.g.
```
let e = error in if x then e else ()
```
In Haskell this is conditionally error, in PLC it is unconditionally error.

These sorts of thing can be written by the user, or generated by GHC.

We solve this by compiling let-bindings *non-strictly*. That means we delay the body
and force all uses of it.

Conveniently, we can just use PIR's support for non-strict let bindings to implement this.
The PIR optimizer (which we use by default) will also strictify any such bindings that
turn out to be pure, so we shouldn't pay any cost for having unnecessary non-strict
bindings.
-}

{- Note [String literals]
String literals are a huge pain. Ultimately, the reason for this is that GHC's 'String' type
is transparently equal to '[Char]', it is *not* opaque.

So we can't just replace GHC's 'String' with PLC's 'String' wholesale. Otherwise things will
behave quite weirdly with things that expect 'String' to be a list. (We want to be type-preserving!)

However, we can get from GHC's 'String' to our 'String' using 'IsString'. This is fine in theory:
we can turn string literals into lists of characters, and then fold over the list adding them
into a big string. But it's bad for two reasons:
- We have to actually do the fold.
- The string literal is there in the generated code as a list of characters, which is pretty big.

So we'd really like to recognize the pattern of applying 'fromString' to a string literal, and then
just use the content of the Haskell string literal to make a PLC string literal.

This is very fiddly:
- Sometimes we get the typeclass method application.
    - But we only want to change it when it's targeting the PLC string type, so we need to have
      that type around so we can check.
- Sometimes the selector has been inlined.
    - We can't easily get access to the name of the method definition itself, so instead we mark
      that as INLINE and look for a special function ('stringToBuiltinString') that is in its
      body (and we use the OPAQUE pragma on that function to ensure it isn't inlined).
- Sometimes our heuristics fail.
    - The actual definition of 'stringToBuiltinString' works, so in the worst case we fall back
      to using it and converting the list of characters into an expression.

It's also annoying since this is the first time that we have to look for a marker function inside
the plugin compilation mode, so we have a special function that's not a builtin (in that it doesn't
just get turned into a function in PLC).
-}

{- Note [Runtime reps]
GHC has the notion of `RuntimeRep`. The kind of types is actually `TYPE rep`, where rep is of kind
`RuntimeRep`. Thus normal types have kind `TYPE LiftedRep`, and unlifted and unboxed types have
various other fancy kinds.

We don't have different runtime representations. But we can make the observation that for things
which say they should have a different runtime representation... we can just represent them as
normal lifted types. In particular, this lets us represent unboxed tuples as normal tuples, which
is helpful, since GHC will often produce these when it transforms the program.

That gives us a strategy for `RuntimeRep`
- Compile `TYPE rep` as `Type`, regardless of what `rep` is
- Ignore binders that bind types of kind `RuntimeRep`, assuming that those will only ever be used
  in a `Type rep` where we are going to ignore the rep anyway.
    - Note that binders for types of kind runtime rep binders can appear in both types and kinds!
- Ignore applications to types of kind `RuntimeRep`, since we're ignoring the binders.

Doing this thoroughly means also ignoring them in types, type constructors, and data constructors,
which is a bit more involved, see e.g.
- 'dropRuntimeRepVars' in 'compileTyCon' to ignore 'RuntimeRep' type variables
- 'dropRuntimeRepArgs' in 'compileType' and 'getMatchInstantiated'
-}

{- Note [Dependency tracking]
We use the PIR support for creating a whole bunch of definitions with dependencies between them, and then generating the code with them all
in the right order. However, this requires us to know what the dependencies of a definition *are*.

There are broadly two ways we could do this:
1. Statically determine before compiling a term/type what it depends on (e.g. by looking at the free variables in the input Core).
2. Dynamically track dependencies as we compile a term/type; whenever we see a reference to something, add it as a dependency.

We used to do the former but we now do the latter. The reason for this is that we sometimes generate bits of code
dynamically as we go. For example, the boolean coverage code *adds* some calls to 'traceBool' into the program. That means
we need a dependency on 'traceBool' - but it wasn't there at the beginning, so a static approach won't work.

The dynamic approach requires us to:
1. Track the current definition.
2. Ensure that the definition is tracked while we are recording things it may depend on (this may require creating a fake definition to begin with)
3. Record dependencies when we find them.

This typically means that we do a three-step process for a given definition:
1. Create a definition with a fake body (this is often also needed for recursion, see Note [Occurrences of recursive names])
2. Compile the real body (during which point dependencies are discovered and added to the fake definition).
3. Modify the definition with the real body.
-}

{- Note [Occurrence analysis]
GHC has "occurrence analysis", which is quite handy. In particular, it can tell you if variables are dead, which is useful
in a couple of places.

But it typically gets run *before* the simplifier, so when we get the expression we might be missing occurence analysis
for any variables that were freshly created by the simplifier. That's easy to fix: we just run the occurrence analyser
ourselves before we start.
-}

hoistExpr
  :: (CompilingDefault uni fun m ann)
  => GHC.Var
  -> GHC.CoreExpr
  -> m (PIRTerm uni fun)
hoistExpr var t = do
  wrapUnsafeDataAsConstrName <-
    lookupGhcName 'PlutusTx.AsData.Internal.wrapUnsafeDataAsConstr
  let name = GHC.getName var
      lexName = LexName name

      -- See Note [Compiling AsData Matchers and Their Invocations]
      isAsDataMatcher =
        any
          ((== wrapUnsafeDataAsConstrName) . GHC.getName @GHC.Var)
          (universeBi t)
      -- If the original ID has an "always inline" pragma, then
      -- propagate that to PIR so that the PIR inliner will deal
      -- with it.
      ann =
        (if hasAlwaysInlinePragma var then annAlwaysInline else annMayInline)
          { annIsAsDataMatcher = isAsDataMatcher
          }
  -- See Note [Dependency tracking]
  modifyCurDeps (Set.insert lexName)
  maybeDef <- PIR.lookupTerm lexName
  let addSpan = case getVarSourceSpan var of
        Nothing  -> id
        Just src -> fmap . fmap . addSrcSpan $ src ^. srcSpanIso
  case maybeDef of
    Just term -> pure term
    -- See Note [Dependency tracking]
    Nothing -> withCurDef lexName . traceCompilation 1 ("Compiling definition of:" GHC.<+> GHC.ppr var) $ do
      var' <- compileVarFresh ann var
      -- See Note [Occurrences of recursive names]
      PIR.defineTerm
        lexName
        (PIR.Def var' (PIR.mkVar var', PIR.Strict))
        mempty

      t' <- maybeProfileRhs var var' =<< addSpan (compileExpr t)
      -- See Note [Non-strict let-bindings]
      PIR.modifyTermDef lexName (const $ PIR.Def var' (t', PIR.NonStrict))
      pure $ PIR.mkVar var'

-- 'GHC.Var' in argument is only for extracting srcspan and accurate name.
maybeProfileRhs
  :: (CompilingDefault uni fun m ann)
  => GHC.Var
  -> PLCVar uni
  -> PIRTerm uni fun
  -> m (PIRTerm uni fun)
maybeProfileRhs ghcVar var t = do
  CompileContext{ccOpts = compileOpts} <- ask
  let
    nameStr = GHC.occNameString $ GHC.occName $ GHC.varName $ ghcVar
    displayName = T.pack $
      case getVarSourceSpan ghcVar of
        -- When module is not compiled and GHC is using cached build from previous build, it will
        -- lack source span. There's nothing much we can do about this here since this is GHC
        -- behavior. Issue #7203
        Nothing  -> nameStr
        Just src -> nameStr <> " (" <> show (src ^. srcSpanIso) <> ")"

    ty = PLC._varDeclType var
    isFunctionOrAbstraction = case ty of PLC.TyFun{} -> True; PLC.TyForall{} -> True; _ -> False
  -- Trace only if profiling is on *and* the thing being defined is a function
  if coProfile compileOpts == All && isFunctionOrAbstraction
    then do
      thunk <- PLC.freshName "thunk"
      pure $ entryExitTracingInside thunk displayName t ty
    else pure t

mkTrace
  :: (uni `PLC.HasTermLevel` T.Text)
  => PLC.Type PLC.TyName uni Ann
  -> T.Text
  -> PIRTerm uni PLC.DefaultFun
  -> PIRTerm uni PLC.DefaultFun
mkTrace ty str v =
  PLC.mkIterApp
    (PIR.TyInst annMayInline (PIR.Builtin annMayInline PLC.Trace) ty)
    ((annMayInline,) <$> [PLC.mkConstant annMayInline str, v])

-- `mkLazyTrace ty str v` builds the term `force (trace str (delay v))` if `v` has type `ty`
mkLazyTrace
  :: (CompilingDefault uni fun m ann)
  => PLC.Type PLC.TyName uni Ann
  -> T.Text
  -> PIRTerm uni PLC.DefaultFun
  -> m (PIRTerm uni fun)
mkLazyTrace ty str v = do
  delayedBody <- delay v
  delayedType <- delayType ty
  force $ mkTrace delayedType str delayedBody

{- Note [Profiling polymorphic functions]
In order to profile polymorphic functions, we have to go under the type abstractions.
But we also need the type of the final inner term in order to construct the correct
invocations of 'trace'. At the moment we get this from the *type* of the term.

But this goes wrong as soon as there are type variables involved!

id :: forall a . a -> a
id = /\a . \(x :: a) -> x -- The 'a' here is not the same as the 'a' in the type signature!

The type of the term needs to use the type variables bound by the type abstractions,
not the ones bound by the foralls in the type signature.

We sort this out in a hacky way by continuing to use the type of the overall term, but
constructing a substitution from the type-bound variables to the term-bound variables,
and then applying that at the end. Not pleasant, but it works.

Note that creating a substitution with a map relies on globally unique names in types.
But that's okay, because these are all types we've been creating just now in Quote, so
we should have globally unique names
-}

{- Note [Term/type argument mismatches]
Given a term t and its type ty we can process them in parallel popping off arguments/function types.

But we can end up with a mismatch:
- We run out of arguments at the term level e.g. because we see something like `(\x -> \y -> y) 1`,
which is of function type but isn't a lambda until you reduce.
- We run out of arguments at the type level e.g. because we see something like `(\a -> (a -> a)) b`,
which is a function type but isn't a function type until you reduce.

It's usually okay to stop at this point, since the remaining things usually aren't "proper" arguments.
In the term case, it's a lambda computed by an application, which won't occur from a "proper" argument.
In the type case, we only generate type lambdas for newtypes, which will block "proper" arguments anyway,
i.e. it comes from something like this:

f :: Identity (a -> a)
f = Identity (\x -> x)
-}

{-| Add entry/exit tracing inside a term's leading arguments, both term and type arguments.
@(/\a -> \b -> body)@ into @/\a -> \b -> entryExitTracing body@.
-}
entryExitTracingInside
  :: PIR.Name
  -> T.Text
  -> PIRTerm PLC.DefaultUni PLC.DefaultFun
  -> PLCType PLC.DefaultUni
  -> PIRTerm PLC.DefaultUni PLC.DefaultFun
entryExitTracingInside lamName displayName = go mempty
 where
  go
    :: Map.Map PLC.TyName (PLCType PLC.DefaultUni)
    -> PIRTerm PLC.DefaultUni PLC.DefaultFun
    -> PLCType PLC.DefaultUni
    -> PIRTerm PLC.DefaultUni PLC.DefaultFun
  go subst (LamAbs ann n t body) (PLC.TyFun _ _dom cod) =
    -- when t = \x -> body, => \x -> entryExitTracingInside body
    LamAbs ann n t $ go subst body cod
  go subst (TyAbs ann tn1 k body) (PLC.TyForall _ tn2 _k ty) =
    -- when t = /\x -> body, => /\x -> entryExitTracingInside body
    -- See Note [Profiling polymorphic functions]
    let subst' = Map.insert tn2 (PLC.TyVar annMayInline tn1) subst
     in TyAbs ann tn1 k $ go subst' body ty
  -- See Note [Term/type argument mismatches]
  -- Even if there still look like there are arguments on the term or the type level, because we've hit
  -- a mismatch we go ahead and insert our profiling traces here.
  go subst e ty =
    -- See Note [Profiling polymorphic functions]
    let ty' = PLC.typeSubstTyNames (\tn -> Map.lookup tn subst) ty
     in entryExitTracing lamName displayName e ty'

{- Note [Profiling Markers]
   The @profile-all@ will insert trarces when entering and exciting functions. These
   traces have a string marker to indicate that a given traces message is for enter/exit
   marking. Markers are just simple strings: "->" and "<-". So for any reason in the
   future this marker needs to be changed, all of utilities that uses this marker will
   need to be updated.

   This list will track of all of the utilities that uses this marker:
   - plutus-core:traceToStacks
   - @UntypedPlutusCore.Evaluation.Machine.Cek.EmitterMode.logWithCallTraceEmitter@
-}

-- | Add tracing before entering and after exiting a term.
entryExitTracing
  :: PLC.Name
  -> T.Text
  -> PIRTerm PLC.DefaultUni PLC.DefaultFun
  -> PLC.Type PLC.TyName PLC.DefaultUni Ann
  -> PIRTerm PLC.DefaultUni PLC.DefaultFun
entryExitTracing lamName displayName e ty =
  let defaultUnitTy = PLC.TyBuiltin annMayInline (PLC.SomeTypeIn PLC.DefaultUniUnit)
      defaultUnit = PIR.Constant annMayInline (PLC.someValueOf PLC.DefaultUniUnit ())
   in -- (trace @(() -> c) "entering f" (\() -> trace @c "exiting f" body) ())
      PIR.Apply
        annMayInline
        ( mkTrace
            (PLC.TyFun annMayInline defaultUnitTy ty) -- ()-> ty
            -- See Note [Profiling Marker]
            ("-> " <> displayName)
            -- \() -> trace @c "exiting f" e
            (LamAbs annMayInline lamName defaultUnitTy (mkTrace ty ("<- " <> displayName) e))
        )
        defaultUnit

-- Expressions

{- Note [Tracking coverage and lazyness]
   When we insert a coverage annotation `a` that is meant to be collected when we execute
   `a` we would like do something like `trace (show a) body`. However, we can't do this
   because `body` may throw an exception and that would in turn cause `show a` never to be logged.
   To get around this we instead generate the code `force (trace (show a) (delay body))` to
   guarantee that the annotation `a` is logged before we execute `body`.
-}

{- Note [Boolean coverage]
   During testing it is useful (sometimes even critical) to know which boolean
   expressions have evaluated to true and false respectively. To track this we
   introduce `traceBool "<expr evaluated to True>" "<expr evaluated to False>" expr`
   around every non-constructor boolean typed expression `expr` with a known source location
   when boolean coverage is turned on.

   The annotation `<expr evaluated to True>` is implemented by adding a `CoverBool location True`
   coverage annotation with the head function in `expr` as metadata. This means that in an
   expression like: `foo x < bar y && all isGood xs`

   We will get annotations for `&&`, `<`, `all`, and `isGood` (given that `isGood` is defined in a
   module with coverage turned on).
-}

{- Note [GHC.Magic.noinline]
   For some functions we have two conflicting desires:
   - We want to have the unfolding available for the plugin.
   - We don't want the function to *actually* get inlined before the plugin runs, since we rely
   on being able to see the original function for some reason.

   'INLINABLE' achieves the first, but may cause the function to be inlined too soon.

   We can solve this at specific call sites by using the 'noinline' magic function from
   GHC. This stops GHC from inlining it. As a bonus, it also won't be inlined if
   that function is compiled later into the body of another function.

   We do therefore need to handle 'noinline' in the plugin, as it itself does not have
   an unfolding.
-}

compileExpr :: (CompilingDefault uni fun m ann) => GHC.CoreExpr -> m (PIRTerm uni fun)
compileExpr e = traceCompilation 2 ("Compiling expr:" GHC.<+> GHC.ppr e) $ do
  -- See Note [Scopes]
  CompileContext
    { ccScope = scope
    , ccModBreaks = maybeModBreaks
    , ccBuiltinsInfo = binfo
    , ccSafeToInline = safeToInline
    } <-
    ask

  -- TODO: Maybe share this to avoid repeated lookups. Probably cheap, though.
  builtinIntegerTyCon <- lookupGhcTyCon ''BI.BuiltinInteger
  builtinBoolTyCon <- lookupGhcTyCon ''BI.BuiltinBool
  builtinDataTyCon <- lookupGhcTyCon ''Builtins.BuiltinData
  builtinPairTyCon <- lookupGhcTyCon ''BI.BuiltinPair
  stringTyName <- lookupGhcName ''Builtins.BuiltinString
  stringToBuiltinStringName <- lookupGhcName 'Builtins.stringToBuiltinString
  builtinByteStringTyName <- lookupGhcName ''Builtins.BuiltinByteString
  stringToBuiltinByteStringName <- lookupGhcName 'Builtins.stringToBuiltinByteString
  builtinByteStringHexTyName <- lookupGhcName ''Builtins.BuiltinByteStringHex
  builtinByteStringUtf8TyName <- lookupGhcName ''Builtins.BuiltinByteStringUtf8
  stringToBuiltinByteStringUtf8Name <- lookupGhcName 'Builtins.stringToBuiltinByteStringUtf8
  stringToBuiltinByteStringHexName <- lookupGhcName 'Builtins.stringToBuiltinByteStringHex
  useToOpaqueName <- lookupGhcName 'Builtins.useToOpaque
  useFromOpaqueName <- lookupGhcName 'Builtins.useFromOpaque
  mkNilOpaqueName <- lookupGhcName 'Builtins.mkNilOpaque
  boolOperatorOr <- lookupGhcName '(PlutusTx.Bool.||)
  boolOperatorAnd <- lookupGhcName '(PlutusTx.Bool.&&)
  inlineName <- lookupGhcName 'PlutusTx.Optimize.Inline.inline

  case e of
    {- Note [Lazy boolean operators]
      (||) and (&&) have a special treatment: we want them lazy in the second argument,
      as this is the behavior in Haskell and other PLs.
      Covered by this spec: plutus-tx-plugin/test/ShortCircuit/Spec.hs
    -}
    -- Lazy ||
    GHC.App (GHC.App (GHC.Var var) a) b
      | GHC.getName var == boolOperatorOr ->
          compileExpr $ GHC.mkIfThenElse a (GHC.Var GHC.trueDataConId) b
    -- Lazy &&
    GHC.App (GHC.App (GHC.Var var) a) b
      | GHC.getName var == boolOperatorAnd ->
          compileExpr $ GHC.mkIfThenElse a b (GHC.Var GHC.falseDataConId)
    -- `inline f` or `inline (f x  ... xn)`
    GHC.App (GHC.App (GHC.Var var) (GHC.Type _aTy)) e'
      | GHC.getName var == inlineName || GHC.getName var == GHC.inlineIdName ->
          case GHC.collectArgs (strip e') of
            (strip -> GHC.Var f, args) ->
              case GHC.maybeUnfoldingTemplate (GHC.realIdUnfolding f) of
                Nothing ->
                  case lookupName scope (GHC.getName f) of
                    -- If `f` is locally bound, and its definition has already been compiled,
                    -- we use it directly.
                    -- This only supports `inline f`, not `inline (f x1 ... xn)`.
                    Just (_var, Just def) | null args -> pure def
                    _                                 -> compileExpr e'
                Just unfolding
                  -- `f` is recursive. We do not inline recursive bindings.
                  | any (== f) (universeBi unfolding) -> compileExpr e'
                  | otherwise -> compileExpr (GHC.mkCoreApps unfolding args)
            _ -> compileExpr e'
    -- See Note [String literals]
    -- See Note [IsString instances and UTF-8 encoded string literals]
    -- IsString has only one method, so it's enough to know that it's an IsString method
    -- to know we're looking at fromString.
    -- We can safely commit to this match as soon as we've seen fromString -
    -- we won't accept any applications of fromString that aren't creating literals of
    -- the types we support.
    (strip -> GHC.Var (GHC.idDetails -> GHC.ClassOpId cls))
      `GHC.App` GHC.Type ty
      `GHC.App` _dict
      `GHC.App` (strip -> content)
        | GHC.getName cls == GHC.isStringClassName -> do
            let throwUnsupported =
                  throwSd UnsupportedError $
                    ""
                      GHC.$$ "Use of fromString is only supported for the following types:"
                      GHC.$$ "-"
                      GHC.<+> GHC.ppr stringTyName
                      GHC.$$ "-"
                      GHC.<+> GHC.ppr builtinByteStringTyName
                      GHC.$$ "-"
                      GHC.<+> GHC.ppr builtinByteStringHexTyName
                      GHC.$$ "-"
                      GHC.<+> GHC.ppr builtinByteStringUtf8TyName
                      GHC.$$ ""
                      GHC.$$ "Using fromString for"
                      GHC.<+> GHC.ppr ty
                      GHC.<+> "is not supported."
            case GHC.tyConAppTyCon_maybe ty of -- extract Type constructor without arguments
            -- BuiltinByteString
              Just tyCtor | GHC.getName tyCtor == builtinByteStringTyName -> do
                bytes <- stringLiteralAsBytes builtinByteStringTyName content
                PIR.Constant annMayInline . PLC.someValue <$> utf8CodePointsAsBytes bytes
              -- BuiltinByteStringUtf8
              Just tyCtor
                | GHC.getName tyCtor == builtinByteStringUtf8TyName ->
                    PIR.Constant annMayInline . PLC.someValue
                      <$> stringLiteralAsBytes builtinByteStringUtf8TyName content
              -- BuiltinByteStringHex
              Just tyCtor | GHC.getName tyCtor == builtinByteStringHexTyName -> do
                hexBytes <- stringLiteralAsBytes builtinByteStringHexTyName content
                case Base16.decode hexBytes of
                  Left err -> throwSd UnsupportedError $ "Invalid hex encoding:" GHC.<+> GHC.text err
                  Right bs -> pure $ PIR.Constant annMayInline $ PLC.someValue bs
              -- BuiltinString
              Just tyCtor
                | GHC.getName tyCtor == stringTyName ->
                    PIR.Constant annMayInline . PLC.someValue
                      <$> stringLiteralAsText stringTyName content
              -- For other unsupported types we have to fail compilation here,
              -- because it won't succeed anyway:
              -- 'fromString' function contains 'Data.Char' type in its definition
              -- and plugin can't compile it.
              _ -> throwUnsupported

    -- 'stringToBuiltinByteString' invocation
    (strip -> GHC.Var n) `GHC.App` (strip -> content)
      | GHC.getName n == stringToBuiltinByteStringName -> do
          bytes <- stringLiteralAsBytes builtinByteStringTyName content
          PIR.Constant annMayInline . PLC.someValue <$> utf8CodePointsAsBytes bytes

    -- 'stringToBuiltinByteStringUtf8' invocation
    (strip -> GHC.Var n) `GHC.App` (strip -> content)
      | GHC.getName n == stringToBuiltinByteStringUtf8Name ->
          PIR.Constant annMayInline . PLC.someValue
            <$> stringLiteralAsBytes builtinByteStringTyName content
    -- 'stringToBuiltinByteStringHex' invocation
    (strip -> GHC.Var n) `GHC.App` (strip -> content)
      | GHC.getName n == stringToBuiltinByteStringHexName -> do
          hexBytes <- stringLiteralAsBytes builtinByteStringHexTyName content
          case Base16.decode hexBytes of
            Left err -> throwSd UnsupportedError $ "Invalid hex encoding:" GHC.<+> GHC.text err
            Right bs -> pure $ PIR.Constant annMayInline $ PLC.someValue bs

    -- 'stringToBuiltinString' invocation
    (strip -> GHC.Var n) `GHC.App` (strip -> arg)
      | GHC.getName n == stringToBuiltinStringName ->
          PIR.Constant annMayInline . PLC.someValue <$> stringLiteralAsText stringTyName arg
    -- See Note [Literals]
    GHC.Lit lit -> compileLiteral lit
    -- These are all wrappers around string and char literals, but keeping them allows us to give better errors
    -- unpackCString# is just a wrapper around a literal
    GHC.Var n `GHC.App` expr | GHC.getName n == GHC.unpackCStringName -> compileExpr expr
    -- See Note [unpackFoldrCString#]
    GHC.Var build `GHC.App` _ `GHC.App` GHC.Lam _ (GHC.Var unpack `GHC.App` _ `GHC.App` expr)
      | GHC.getName build == GHC.buildName && GHC.getName unpack == GHC.unpackCStringFoldrName ->
          compileExpr expr
    -- C# is just a wrapper around a literal
    GHC.Var (GHC.idDetails -> GHC.DataConWorkId dc) `GHC.App` arg | dc == GHC.charDataCon -> compileExpr arg
    -- Handle constructors of 'Integer'
    GHC.Var (GHC.idDetails -> GHC.DataConWorkId dc) `GHC.App` arg | GHC.dataConTyCon dc == GHC.integerTyCon -> do
      i <- compileExpr arg
      -- IN is a negative integer!
      if GHC.dataConName dc == GHC.integerINDataConName
        then do
          negateTerm <- lookupIntegerNegate
          pure $ PIR.mkIterApp negateTerm [(annMayInline, i)]
        else pure i
    -- Unboxed unit, (##).
    GHC.Var (GHC.idDetails -> GHC.DataConWorkId dc) | dc == GHC.unboxedUnitDataCon -> pure (PIR.mkConstant annMayInline ())
    -- Ignore the magic 'noinline' function, it's the identity but has no unfolding.
    -- See Note [GHC.Magic.noinline]
    GHC.Var n `GHC.App` GHC.Type _ `GHC.App` arg | GHC.getName n == GHC.noinlineIdName -> compileExpr arg
    -- See Note [GHC runtime errors]
    -- <error func> <runtime rep> <overall type> <call stack> <message>
    GHC.Var (isErrorId -> True) `GHC.App` _ `GHC.App` GHC.Type t `GHC.App` _ `GHC.App` _ ->
      PIR.TyInst annMayInline <$> errorFunc <*> compileTypeNorm t
    -- <error func> <runtime rep> <overall type> <message>
    GHC.Var (isErrorId -> True) `GHC.App` _ `GHC.App` GHC.Type t `GHC.App` _ ->
      PIR.TyInst annMayInline <$> errorFunc <*> compileTypeNorm t
    -- <error func> <overall type> <message>
    GHC.Var (isErrorId -> True) `GHC.App` GHC.Type t `GHC.App` _ ->
      PIR.TyInst annMayInline <$> errorFunc <*> compileTypeNorm t
    (strip -> GHC.Var n) `GHC.App` GHC.Type ty
      | GHC.getName n == mkNilOpaqueName -> case ty of
          GHC.TyConApp tyCon []
            | tyCon == GHC.integerTyCon || tyCon == builtinIntegerTyCon ->
                pure $ PLC.mkConstant annMayInline ([] @Integer)
            | tyCon == builtinBoolTyCon -> pure $ PLC.mkConstant annMayInline ([] @Bool)
            | tyCon == builtinDataTyCon -> pure $ PLC.mkConstant annMayInline ([] @PLC.Data)
          GHC.TyConApp tyCon [GHC.TyConApp tyArg1 [], GHC.TyConApp tyArg2 []]
            | (tyCon, tyArg1, tyArg2) == (builtinPairTyCon, builtinDataTyCon, builtinDataTyCon) ->
                pure $ PLC.mkConstant annMayInline ([] @(PLC.Data, PLC.Data))
          _ -> throwPlain $ CompilationError "'mkNil' applied to an unknown type"
    GHC.Var n
      | GHC.getName n == useToOpaqueName ->
          throwPlain $
            UnsupportedError "It is no longer possible to use 'toBuiltin' with a script, use 'toOpaque' instead"
    GHC.Var n
      | GHC.getName n == useFromOpaqueName ->
          throwPlain $
            UnsupportedError
              "It is no longer possible to use 'fromBuiltin' with a script, use 'fromOpaque' instead"
    -- See Note [Uses of Eq]
    GHC.Var n
      | GHC.getName n == GHC.eqName ->
          throwPlain $ UnsupportedError "Use of == from the Haskell Eq typeclass"
    GHC.Var n
      | isProbablyIntegerEq n -> do
          lookupGhcId 'Builtins.equalsInteger >>= compileExpr . GHC.Var
    GHC.Var n
      | isProbablyBytestringEq n ->
          throwPlain $
            UnsupportedError "Use of Haskell ByteString equality, possibly via the Haskell Eq typeclass"
    GHC.Var n
      -- Try to produce a sensible error message if a range like [1..9] is encountered.  This works
      -- by looking for occurrences of GHC.Enum.enumFromTo and similar functions; the same error
      -- occurs if these functions are used explicitly.
      | isProbablyBoundedRange n ->
          throwPlain $
            UnsupportedError $
              T.pack
                ( "Use of enumFromTo or enumFromThenTo, possibly via range syntax. "
                    ++ "Please use PlutusTx.Enum.enumFromTo or PlutusTx.Enum.enumFromThenTo instead."
                )
    -- Throw an error if we find an infinite range like [1..]
    GHC.Var n
      | isProbablyUnboundedRange n ->
          throwPlain $
            UnsupportedError $
              T.pack
                ( "Use of enumFrom or enumFromThen, possibly via range syntax. "
                    ++ "Unbounded ranges are not supported."
                )
    -- locally bound vars
    GHC.Var (lookupName scope . GHC.getName -> Just (var, _def)) -> pure $ PIR.mkVar var
    -- Special kinds of id
    GHC.Var (GHC.idDetails -> GHC.DataConWorkId dc) -> compileDataConRef dc
    -- Class ops don't have unfoldings in general (although they do if they're for one-method classes, so we
    -- want to check the unfoldings case first), see GHC:Note [ClassOp/DFun selection] for why. That
    -- means we have to reconstruct the RHS ourselves, though, which is a pain.
    GHC.Var n@(GHC.idDetails -> GHC.ClassOpId cls) -> do
      -- This code (mostly) lifted from MkId.mkDictSelId, which makes unfoldings for those dictionary
      -- selectors that do have them
      let sel_names = fmap GHC.getName (GHC.classAllSelIds cls)
      val_index <- case elemIndex (GHC.getName n) sel_names of
        Just i  -> pure i
        Nothing -> throwSd CompilationError $ "Id not in class method list:" GHC.<+> GHC.ppr n
      let rhs = GHC.mkDictSelRhs cls val_index

      hoistExpr n rhs
    GHC.Var n -> do
      -- Defined names, including builtin names
      let lexName = LexName $ GHC.getName n
      modifyCurDeps (\d -> Set.insert lexName d)
      maybeDef <- PIR.lookupTerm lexName
      case maybeDef of
        Just term -> pure term
        Nothing ->
          -- No other cases apply; compile the unfolding of the var
          case GHC.maybeUnfoldingTemplate (GHC.realIdUnfolding n) of
            -- See Note [Unfoldings]
            -- The "unfolding template" includes things with normal unfoldings and also dictionary functions
            Just unfolding -> hoistExpr n unfolding
            Nothing ->
              throwSd FreeVariableError $
                "Variable"
                  GHC.<+> GHC.ppr n
                  GHC.$+$ (GHC.ppr $ GHC.idDetails n)
                  GHC.$+$ (GHC.ppr $ GHC.realIdUnfolding n)
    -- arg can be a type here, in which case it's a type instantiation
    l `GHC.App` GHC.Type t -> do
      l' <- compileExpr l
      fmap
        ( -- If the head of the application is an `AsData` matcher, propagate the
          -- `annIsAsDataMatcher` annotation to the whole application.
          -- See Note [Compiling AsData Matchers and Their Invocations]
          if annIsAsDataMatcher (PIR.termAnn l')
            then fmap (\ann -> ann{annIsAsDataMatcher = True})
            else id
        )
        ( -- Ignore applications to types of 'RuntimeRep' kind, see Note [Runtime reps]
          if GHC.isRuntimeRepKindedTy t
            then pure l'
            else PIR.TyInst annMayInline <$> pure l' <*> compileTypeNorm t
        )
    -- otherwise it's a normal application
    l `GHC.App` arg -> do
      l' <- compileExpr l
      let isAsDataMatcher = annIsAsDataMatcher (PIR.termAnn l')
      fmap
        ( -- If the head of the application is an `AsData` matcher, propagate the
          -- `annIsAsDataMatcher` annotation to the whole application.
          -- See Note [Compiling AsData Matchers and Their Invocations]
          if isAsDataMatcher
            then fmap (\ann -> ann{annIsAsDataMatcher = True})
            else id
        )
        ( -- If the head of the application is an `AsData` matcher, set `safeToInline`
          -- to True and continue.
          (if isAsDataMatcher then local (\c -> c{ccSafeToInline = True}) else id)
            (PIR.Apply annMayInline <$> pure l' <*> compileExpr arg)
        )
    -- if we're biding a type variable it's a type abstraction
    GHC.Lam b@(GHC.isTyVar -> True) body ->
      -- Ignore type binders for runtime rep variables, see Note [Runtime reps]
      if GHC.isRuntimeRepTy $ GHC.varType b
        then compileExpr body
        else mkTyAbsScoped b $ compileExpr body
    -- otherwise it's a normal lambda
    GHC.Lam b body -> do
      let ann = if safeToInline then annSafeToInline else annMayInline
      mkLamAbsScoped ann b $ compileExpr body
    GHC.Let (GHC.NonRec b rhs) body -> do
      -- the binding is in scope for the body, but not for the arg
      rhs' <- compileExpr rhs
      ty <- case rhs of
        GHC.Lit (GHC.LitNumber{})
          | GHC.eqType (GHC.varType b) GHC.byteArrayPrimTy ->
              -- Handle the following case:
              --
              -- ```PlutusTx
              -- let !x = 12345678901234567890
              -- in PlutusTx.equalsInteger x y
              -- ```
              --
              -- ```GHC Core
              -- let {
              --   x_sfhW :: ByteArray#
              --   x_sfhW = 12345678901234567890 } in
              -- equalsInteger (IP x_sfhW) y_X0
              -- ```
              --
              -- What we do here is ignoring the `ByteArray#`, and pretending that
              -- `12345678901234567890` is an Integer.
              pure $ PIR.mkTyBuiltin @_ @Integer @PLC.DefaultUni annMayInline
        _ -> compileTypeNorm $ GHC.varType b
      -- See Note [Non-strict let-bindings]
      withVarTyScoped b ty $ \v -> do
        rhs'' <- maybeProfileRhs b v rhs'
        let binds = pure $ PIR.TermBind annMayInline PIR.NonStrict v rhs''
        body' <- compileExpr body
        pure $ PIR.Let annMayInline PIR.NonRec binds body'
    GHC.Let (GHC.Rec bs) body ->
      withVarsScoped (fmap (second (const Nothing)) bs) $ \vars -> do
        -- the bindings are scope in both the body and the args
        -- TODO: this is a bit inelegant matching the vars back up
        binds <- for (zip vars bs) $ \(v, (ghcVar, rhs)) -> do
          rhs' <- maybeProfileRhs ghcVar v =<< compileExpr rhs
          -- See Note [Non-strict let-bindings]
          pure $ PIR.TermBind annMayInline PIR.NonStrict v rhs'
        body' <- compileExpr body
        pure $ PIR.mkLet annMayInline PIR.Rec binds body'
    GHC.Case scrutinee b t alts ->
      compileCase (const . GHC.isDeadOcc . GHC.occInfo . GHC.idInfo) True binfo scrutinee b t alts
    -- we can use source notes to get a better context for the inner expression
    -- these are put in when you compile with -g
    -- See Note [What source locations to cover]
    GHC.Tick tick body | Just src <- getSourceSpan maybeModBreaks tick ->
      traceCompilation 1 ("Compiling expr at:" GHC.<+> GHC.ppr src) $ do
        CompileContext{ccOpts = coverageOpts} <- ask
        -- See Note [Coverage annotations]
        let anns = Set.toList $ activeCoverageTypes coverageOpts
        compiledBody <- fmap (addSrcSpan $ src ^. srcSpanIso) <$> compileExpr body
        foldM (coverageCompile body (GHC.exprType body) src) compiledBody anns

    -- ignore other annotations
    GHC.Tick _ body -> compileExpr body
    -- See Note [Coercions and newtypes]
    GHC.Cast body _ -> compileExpr body
    GHC.Type _ -> throwPlain $ UnsupportedError "Types as standalone expressions"
    GHC.Coercion _ -> throwPlain $ UnsupportedError "Coercions as expressions"

compileCase
  :: (CompilingDefault uni fun m ann)
  => (GHC.Var -> GHC.CoreExpr -> Bool)
  -- ^ Whether the variable is dead in the expr
  -> Bool
  -- ^ Whether we should try to rewrite unnecessary constructor applications
  -> BuiltinsInfo uni fun
  -> GHC.CoreExpr
  -> GHC.Var
  -> GHC.Type
  -> [GHC.CoreAlt]
  -> m (PIRTerm uni fun)
compileCase isDead rewriteConApps binfo scrutinee binder t alts = do
  wrapTailName <- lookupGhcName 'PlutusTx.AsData.Internal.wrapTail
  let
    -- See Note [Compiling AsData Matchers and Their Invocations]
    isWrapTailApp =
      case GHC.collectArgs (strip scrutinee) of
        (strip -> GHC.Var f, _args) -> GHC.getName f == wrapTailName
        _                           -> False
    binderAnn
      | hasAlwaysInlinePragma binder = annAlwaysInline
      | isWrapTailApp = annSafeToInline
      | otherwise = annMayInline
  case alts of
    [GHC.Alt con bs body]
      -- See Note [Evaluation-only cases]
      | all (`isDead` body) bs -> do
          -- See Note [At patterns]
          scrutinee' <- compileExpr scrutinee
          withVarScoped binder binderAnn (Just scrutinee') $ \v -> do
            body' <- compileExpr body
            -- See Note [At patterns]
            let binds = [PIR.TermBind annMayInline PIR.Strict v scrutinee']
            pure $ PIR.mkLet annMayInline PIR.NonRec binds body'
      | rewriteConApps
      , GHC.DataAlt dataCon <- con -> do
          -- Attempt to rewrite constructor applications, since sometimes they cannot be
          -- compiled (e.g., opaque constructors).
          -- For example, this rewrites

          -- ```
          -- case scrut of b {BuiltinList xs} -> ...BuiltinList @BuiltinData xs...
          -- ```
          --
          -- into
          --
          -- ```
          -- case scrut of b {BuiltinList xs} -> ...b...
          -- ```
          --
          -- after which `xs` is hopefully dead, and we can then compile it using the
          -- `all (`isDead` body) bs` branch of `compileCase`.
          let f (GHC.collectArgs -> (GHC.Var (GHC.isDataConId_maybe -> Just dataCon'), args0))
                | dataCon == dataCon'
                , -- Discard type arguments
                  let args = mapMaybe (\case GHC.Var v -> Just v; _ -> Nothing) args0
                , length bs == length args
                , and (zipWith (==) bs args) =
                    GHC.Var binder
              f other = other
              -- This time we can no longer use `GHC.isDeadOcc`. Instead we check manually.
              isDead' b = not . any (== b) . universeBi
          -- If some binders are still alive, we have to give up (rather than trying to rewrite
          -- constructor applications again, which will loop), hence `False`.
          compileCase isDead' False binfo scrutinee binder t [GHC.Alt con bs (transform f body)]
    _ -> do
      -- See Note [At patterns]
      scrutinee' <- compileExpr scrutinee
      let scrutineeType = GHC.varType binder

      -- the variable for the scrutinee is bound inside the cases, but not in the scrutinee expression itself
      withVarScoped binder binderAnn (Just scrutinee') $ \v -> do
        (tc, argTys) <- case GHC.splitTyConApp_maybe scrutineeType of
          Just (tc, argTys) -> pure (tc, argTys)
          Nothing ->
            throwSd UnsupportedError $
              "Cannot case on a value of type:" GHC.<+> GHC.ppr scrutineeType
        dcs <- getDataCons tc

        -- it's important to instantiate the match before alts compilation
        match <- getMatchInstantiated scrutineeType
        let matched = PIR.Apply annMayInline match scrutinee'

        let (rest, mdef) = GHC.findDefault alts
        -- This does two things:
        -- 1. Ensure that every set of alternatives has a DEFAULT alt (See Note [We always need DEFAULT])
        -- 2. Compile the body of the DEFAULT alt ahead of time so it can be shared (See Note [Sharing DEFAULT bodies])
        (alts', defCompiled) <- case mdef of
          Just d -> do
            defCompiled <- compileExpr d
            pure (GHC.addDefault rest (Just d), defCompiled)
          Nothing -> do
            let d = GHC.mkImpossibleExpr t "unreachable alternative"
            defCompiled <- compileExpr d
            pure (GHC.addDefault alts (Just d), defCompiled)
        defName <- PLC.freshName "defaultBody"

        -- See Note [Case expressions and laziness]
        compiledAlts <- forM dcs $ \dc -> do
          let alt = GHC.findAlt (GHC.DataAlt dc) alts'
              -- these are the instantiated type arguments, e.g. for the data constructor Just when
              -- matching on Maybe Int it is [Int] (crucially, not [a])
              instArgTys = GHC.scaledThing <$> GHC.dataConInstOrigArgTys dc argTys
          case alt of
            Just a -> do
              -- pass in the body to use for default alternatives, see Note [Sharing DEFAULT bodies]
              (nonDelayedAlt, delayedAlt) <- compileAlt a instArgTys (PIR.Var annMayInline defName)
              return (nonDelayedAlt, delayedAlt)
            Nothing -> throwSd CompilationError $ "No alternative for:" GHC.<+> GHC.ppr dc
        let
          isPureAlt = compiledAlts <&> \(nonDelayed, _) -> PIR.isPure binfo mempty nonDelayed
          lazyCase = not (and isPureAlt || length dcs == 1)
          branches =
            compiledAlts <&> \(nonDelayedAlt, delayedAlt) ->
              if lazyCase then delayedAlt else nonDelayedAlt

        -- See Note [Scott encoding of datatypes]
        -- we need this for the default case body
        originalResultType <- compileTypeNorm t
        -- See Note [Scott encoding of datatypes]
        -- we're going to delay the body, so the matcher needs to be instantiated at the delayed type
        resultType <- maybeDelayType lazyCase originalResultType
        let instantiated = PIR.TyInst annMayInline matched resultType

        let applied = PIR.mkIterApp instantiated $ (annMayInline,) <$> branches
        -- See Note [Case expressions and laziness]
        mainCase <- maybeForce lazyCase applied

        let binds =
              [ -- See Note [At patterns]
                PIR.TermBind annMayInline PIR.NonStrict v scrutinee'
              , -- Bind the default body, see Note [Sharing DEFAULT bodies]
                PIR.TermBind
                  annMayInline
                  PIR.NonStrict
                  (PIR.VarDecl annMayInline defName originalResultType)
                  defCompiled
              ]
        pure $ PIR.mkLet annMayInline PIR.NonRec binds mainCase

{- Note [What source locations to cover]
   We try to get as much coverage information as we can out of GHC. This means that
   anything we find in the GHC Core code that hints at a source location will be
   included as a coverage annotation. This has both advantages and disadvantages.
   On the one hand "trying as hard as we can" gives us as much coverage information as
   possible. On the other hand GHC can sometimes do tricky things like tick floating
   that will degrade the quality of the coverage information we get. However, we have
   yet to find any evidence that GHC treats different ticks differently with regards
   to tick floating.
-}

{- Note [Partial type signature for getSourceSpan]
Why is there a partial type signature here? The answer is that we sometimes compile with a patched
GHC provided from haskell.nix that has a slightly busted patch applied to it. That patch changes
the type of the 'Tickish' part of 'Tick'.

Obviously we would eventually like to not have this problem (should be when we go to 9.2), but in
the mean time we'd like things to compile on both the patched and non-patched GHC.

A partial type signature provides a simple solution: GHC will infer different types for the hole
in each case, but since we operate on them in the same way, there's no problem.
-}

-- See Note [What source locations to cover]
-- See Note [Partial type signature for getSourceSpan]

-- | Do your best to try to extract a source span from a tick
getSourceSpan :: Maybe GHC.ModBreaks -> _ -> Maybe GHC.RealSrcSpan
getSourceSpan _ GHC.SourceNote{GHC.sourceSpan = src} = Just src
getSourceSpan _ GHC.ProfNote{GHC.profNoteCC = cc} =
  case cc of
    GHC.NormalCC _ _ _ (GHC.RealSrcSpan sp _) -> Just sp
    GHC.AllCafsCC _ (GHC.RealSrcSpan sp _)    -> Just sp
    _                                         -> Nothing
getSourceSpan mmb GHC.HpcTick{GHC.tickId = tid} = do
  mb <- mmb
  let arr = GHC.modBreaks_locs mb
      range = Array.bounds arr
  GHC.RealSrcSpan sp _ <- if Array.inRange range tid then Just $ arr Array.! tid else Nothing
  return sp
getSourceSpan _ _ = Nothing

getVarSourceSpan :: GHC.Var -> Maybe GHC.RealSrcSpan
getVarSourceSpan = GHC.srcSpanToRealSrcSpan . GHC.nameSrcSpan . GHC.varName

srcSpanIso :: Iso' GHC.RealSrcSpan SrcSpan
srcSpanIso = iso fromGHC toGHC
 where
  fromGHC sp =
    SrcSpan
      { srcSpanFile = GHC.unpackFS (GHC.srcSpanFile sp)
      , srcSpanSLine = GHC.srcSpanStartLine sp
      , srcSpanSCol = GHC.srcSpanStartCol sp
      , srcSpanELine = GHC.srcSpanEndLine sp
      , srcSpanECol = GHC.srcSpanEndCol sp
      }
  toGHC sp =
    GHC.mkRealSrcSpan
      (GHC.mkRealSrcLoc (fileNameFs sp) (srcSpanSLine sp) (srcSpanSCol sp))
      (GHC.mkRealSrcLoc (fileNameFs sp) (srcSpanELine sp) (srcSpanECol sp))
  fileNameFs = GHC.fsLit . srcSpanFile

-- | Obviously this function computes a GHC.RealSrcSpan from a CovLoc
toCovLoc :: GHC.RealSrcSpan -> CovLoc
toCovLoc sp =
  CovLoc
    (GHC.unpackFS $ GHC.srcSpanFile sp)
    (GHC.srcSpanStartLine sp)
    (GHC.srcSpanEndLine sp)
    (GHC.srcSpanStartCol sp)
    (GHC.srcSpanEndCol sp)

-- Here be dragons:
-- See Note [Tracking coverage and lazyness]
-- See Note [Coverage order]

-- | Annotate a term for coverage
coverageCompile
  :: (CompilingDefault uni fun m ann)
  => GHC.CoreExpr
  -- ^ The original expression
  -> GHC.Type
  -- ^ The type of the expression
  -> GHC.RealSrcSpan
  -- ^ The source location of this expression
  -> PIRTerm uni fun
  -- ^ The current term (this is what we add coverage tracking to)
  -> CoverageType
  -- ^ The type of coverage to do next
  -> m (PIRTerm uni fun)
coverageCompile originalExpr exprType src compiledTerm covT =
  case covT of
    -- Add a location coverage annotation to tell us "we've executed this piece of code"
    LocationCoverage -> do
      ann <- addLocationToCoverageIndex (toCovLoc src)
      ty <- compileTypeNorm exprType
      mkLazyTrace ty (T.pack . show $ ann) compiledTerm

    -- Add two boolean coverage annotations to tell us "this boolean has been True/False respectively"
    -- see Note [Boolean coverage]
    BooleanCoverage -> do
      -- Check if the thing we are compiling is a boolean
      boolName <- lookupGhcName ''Bool
      trueName <- lookupGhcName 'True
      falseName <- lookupGhcName 'False
      let tyHeadName = GHC.getName <$> GHC.tyConAppTyCon_maybe exprType
          headSymName = GHC.getName <$> findHeadSymbol originalExpr
          isTrueOrFalse = case originalExpr of
            GHC.Var v
              | GHC.DataConWorkId dc <- GHC.idDetails v ->
                  GHC.getName dc `elem` [trueName, falseName]
            _ -> False

      if tyHeadName /= Just boolName || isTrueOrFalse
        then return compiledTerm
        else do
          -- Generate the code:
          -- ```
          -- traceBool "<compiledTerm was true>" "<compiledTerm was false>" compiledTerm
          -- ```
          traceBoolCompiled <- compileExpr . GHC.Var =<< lookupGhcId 'traceBool
          let mkMetadata =
                CoverageMetadata
                  . foldMap
                    (Set.singleton . ApplicationHeadSymbol . GHC.getOccString)
          fc <- addBoolCaseToCoverageIndex (toCovLoc src) False (mkMetadata headSymName)
          tc <- addBoolCaseToCoverageIndex (toCovLoc src) True (mkMetadata headSymName)
          pure . PLC.mkIterApp traceBoolCompiled $
            (annMayInline,)
              <$> [ PLC.mkConstant annMayInline (T.pack . show $ tc)
                  , PLC.mkConstant annMayInline (T.pack . show $ fc)
                  , compiledTerm
                  ]
 where
  findHeadSymbol :: GHC.CoreExpr -> Maybe GHC.Id
  findHeadSymbol (GHC.Var n)    = Just n
  findHeadSymbol (GHC.App t _)  = findHeadSymbol t
  findHeadSymbol (GHC.Lam _ t)  = findHeadSymbol t
  findHeadSymbol (GHC.Tick _ t) = findHeadSymbol t
  findHeadSymbol (GHC.Let _ t)  = findHeadSymbol t
  findHeadSymbol (GHC.Cast t _) = findHeadSymbol t
  findHeadSymbol _              = Nothing

hasAlwaysInlinePragma :: GHC.Var -> Bool
hasAlwaysInlinePragma = GHC.isInlinePragma . GHC.idInlinePragma

{-| We cannot compile the unfolding of `GHC.Num.Integer.integerNegate`, which is
important because GHC inserts calls to it when it sees negations, even negations
of literals (unless NegativeLiterals is on, which it usually isn't). So we directly
define a PIR term for it: @integerNegate = \x -> 0 - x@.
-}
defineIntegerNegate :: (CompilingDefault PLC.DefaultUni fun m ann) => m ()
defineIntegerNegate = do
  ghcId <- lookupGhcId 'GHC.Num.Integer.integerNegate
  -- Always inline `integerNegate`.
  -- `let integerNegate = \x -> 0 - x in integerNegate 1 + integerNegate 2`
  -- is much more expensive than `(-1) + (-2)`. The inliner cannot currently
  -- make this transformation without `annAlwaysInline`, because it is not aware
  -- of constant folding.
  var <- compileVarFresh annAlwaysInline ghcId
  let ann = annMayInline
  x <- safeFreshName "x"
  let
    -- body = 0 - x
    body =
      PIR.LamAbs ann x (PIR.mkTyBuiltin @_ @Integer @PLC.DefaultUni ann) $
        PIR.mkIterApp
          (PIR.Builtin ann PLC.SubtractInteger)
          [ (ann, PIR.mkConstant @Integer ann 0)
          , (ann, PIR.Var ann x)
          ]
    def = PIR.Def var (body, PIR.Strict)
  PIR.defineTerm (LexName GHC.integerNegateName) def mempty

defineFix :: (CompilingDefault PLC.DefaultUni fun m ann) => m ()
defineFix = do
  inlineFix <- asks (coInlineFix . ccOpts)
  ghcId <- lookupGhcId 'PlutusTx.Function.fix
  var <- compileVarFresh (if inlineFix then annAlwaysInline else annMayInline) ghcId
  let rhs = annMayInline <$ PlutusCore.StdLib.Data.Function.fix
  let def = PIR.Def var (rhs, PIR.Strict)
  PIR.defineTerm (LexName (GHC.getName ghcId)) def mempty

lookupIntegerNegate :: (Compiling uni fun m ann) => m (PIRTerm uni fun)
lookupIntegerNegate = do
  ghcName <- lookupGhcName 'GHC.Num.Integer.integerNegate
  PIR.lookupTerm (LexName ghcName) >>= \case
    Just t -> pure t
    Nothing ->
      throwPlain $
        CompilationError "Cannot find the definition of integerNegate. Please file a bug report."

compileExprWithDefs
  :: (CompilingDefault uni fun m ann)
  => GHC.CoreExpr
  -> m (PIRTerm uni fun)
compileExprWithDefs e = do
  defineBuiltinTypes
  defineBuiltinTerms
  defineIntegerNegate
  defineFix
  compileExpr e

{- Note [We always need DEFAULT]
GHC can be clever and omit case alternatives sometimes, typically when the typechecker says a case
is impossible due to GADT cleverness or similar.
We can't do this: we always need to put in all the case alternatives. In particular, that means
we always want a DEFAULT case to fall back on if GHC doesn't provide a specific alternative for
a data constructor.
The easiest way to ensure that we always have a DEFAULT case is just to put one in if it's missing.
-}

{- Note [Sharing DEFAULT bodies]
Consider the following program:
```
data A = B | C | D
f a = case a of
  B -> 1
  _ -> 2
```
How many times will the literal 2 appear in the resulting PIR program? Naively... twice!
We need to make all the cases explicit, so that means we actually need to *duplicate*
the default case for every alternative that needs it, i.e. we end up with something more like
```
f a = case a of
  B -> 1
  C -> 2
  D -> 2
```
This should set of alarm bells: any time we duplicate things we can end up with exponential
programs if the construct is nested. And that can happen - one example is that usage of
pattern synonyms tends to generate code like:
```
f a = case pattern_synonym_func1 of
  pat1 -> ...
  _ -> case pattern_synonym_func2 of
    pat2 -> ...
    _ -> case ...
```
So a case expression with 8 pattern synonyms would generate 2^8 copies of the final default
case - pretty bad!
The solution is straightforward: share the default case. That means we produce a program more
like:
```
f a = let defaultBody = 2 in case a of
  B -> 1
  C -> defaultBody
  D -> defaultBody
```
Then the inliner can inline it as appropriate.
-}
