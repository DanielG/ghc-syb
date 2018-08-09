{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{- | "GHC.Syb.Utils" provides common utilities for the Ghc Api,
     either based on Data\/Typeable or for use with Data.Generics
     over Ghc Api types.
-}
module GHC.SYB.Utils where

import Data.Generics

import PprTyThing()
import GHC hiding (moduleName)
import SrcLoc()
#if __GLASGOW_HASKELL__ >= 802
import NameSet(NameSet)
#elif __GLASGOW_HASKELL__ >= 709
import NameSet(NameSet)
#endif

import Control.Monad

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x
  | (const False
      `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- | A variation of 'everything', using a 'GenericQ Bool' to skip
--   parts of the input 'Data'.
--everythingBut :: GenericQ Bool -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
--everythingBut q k z f x
--  | q x       = z
--  | otherwise = foldl k (f x) (gmapQ (everythingBut q k z f) x)


-- Question: how to handle partial results in the otherwise step?
everythingButStaged :: Stage -> (r -> r -> r) -> r -> GenericQ (r,Bool) -> GenericQ r
everythingButStaged stage k z f x
  | (const False
       `extQ` fixity `extQ` nameSet) x = z
  | stop == True = v
  | otherwise = foldl k v (gmapQ (everythingButStaged stage k z f) x)
  where (v, stop) = f x
        nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- | Look up a subterm by means of a maybe-typed filter.
somethingStaged :: Stage -> (Maybe u) -> GenericQ (Maybe u) -> GenericQ (Maybe u)

-- "something" can be defined in terms of "everything"
-- when a suitable "choice" operator is used for reduction
--
somethingStaged stage z = everythingStaged stage orElse z


-- | Apply a monadic transformation at least somewhere.
--
-- The transformation is tried in a top-down manner and descends down if it
-- fails to apply at the root of the term.  If the transformation fails to apply
-- anywhere within the the term, the whole operation fails.
somewhereStaged :: MonadPlus m => Stage -> GenericM m -> GenericM m

somewhereStaged stage f x
  | (const False
       `extQ` fixity `extQ` nameSet) x = mzero
  | otherwise = f x `mplus` gmapMp (somewhereStaged stage f) x
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- ---------------------------------------------------------------------

{-
-- | Apply a transformation everywhere in bottom-up manner
-- Note type GenericT = forall a. Data a => a -> a
everywhereStaged :: Stage
                    -> (forall a. Data a => a -> a)
                    -> (forall a. Data a => a -> a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
--
everywhereStaged stage f -- = f . gmapT (everywhere f)
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) = mzero
  | otherwise = f . gmapT (everywhere stage f)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool
-}


-- | Monadic variation on everywhere
everywhereMStaged :: Monad m => Stage -> GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereMStaged stage f x
  | (const False
       `extQ` fixity `extQ` nameSet) x = return x
  | otherwise = do x' <- gmapM (everywhereMStaged stage f) x
                   f x'
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool
