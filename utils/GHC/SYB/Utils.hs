{-# LANGUAGE CPP #-}
{- | "GHC.Syb.Utils" provides common utilities for the Ghc Api,
     either based on Data\/Typeable or for use with Data.Generics
     over Ghc Api types.

example output of 'showData' on 'parsedSource', 'renamedSource', and
'typecheckedSource' for a trivial @HelloWorld@ module, compared with
'ppr' output:

@
------------------------- pretty-printed parsedSource
module HelloWorld where
main = putStrLn "Hello, World!"
------------------------- pretty-printed renamedSource
Just (HelloWorld.main = System.IO.putStrLn "Hello, World!",
      [import Prelude],
      Nothing,
      Nothing,

(HaddockModInfo
 (Nothing)
 (Nothing)
 (Nothing)
 (Nothing)))
------------------------- pretty-printed typecheckedSource
Just <AbsBinds [] [] [HelloWorld.main <= [] main]
        HelloWorld.main :: GHC.IOBase.IO ()
        []
        { main = System.IO.putStrLn "Hello, World!" }>
------------------------- shown parsedSource

(L {HelloWorld.hs:1:0}
 (HsModule
  (Just
   (L {HelloWorld.hs:1:7-16} {ModuleName: HelloWorld}))
  (Nothing)
  []
  [
   (L {HelloWorld.hs:2:0-30}
    (ValD
     (FunBind
      (L {HelloWorld.hs:2:0-3}
       (Unqual {OccName: main}))
      (False)
      (MatchGroup
       [
        (L {HelloWorld.hs:2:0-30}
         (Match
          []
          (Nothing)
          (GRHSs
           [
            (L {HelloWorld.hs:2:7-30}
             (GRHS
              []
              (L {HelloWorld.hs:2:7-30}
               (HsApp
                (L {HelloWorld.hs:2:7-14}
                 (HsVar
                  (Unqual {OccName: putStrLn})))
                (L {HelloWorld.hs:2:16-30}
                 (HsLit
                  (HsString {FastString: "Hello, World!"})))))))]
           (EmptyLocalBinds))))] {!type placeholder here?!})
      (WpHole) {!NameSet placeholder here!}
      (Nothing))))]
  (Nothing)
  (HaddockModInfo
   (Nothing)
   (Nothing)
   (Nothing)
   (Nothing))
  (Nothing)))
------------------------- shown renamedSource

((,,,,)
 (HsGroup
  (ValBindsOut
   [
    ((,)
     (NonRecursive) {Bag(Located (HsBind Name)):
     [
      (L {HelloWorld.hs:2:0-30}
       (FunBind
        (L {HelloWorld.hs:2:0-3} {Name: HelloWorld.main})
        (False)
        (MatchGroup
         [
          (L {HelloWorld.hs:2:0-30}
           (Match
            []
            (Nothing)
            (GRHSs
             [
              (L {HelloWorld.hs:2:7-30}
               (GRHS
                []
                (L {HelloWorld.hs:2:7-30}
                 (HsApp
                  (L {HelloWorld.hs:2:7-14}
                   (HsVar {Name: System.IO.putStrLn}))
                  (L {HelloWorld.hs:2:16-30}
                   (HsLit
                    (HsString {FastString: "Hello, World!"})))))))]
             (EmptyLocalBinds))))] {!type placeholder here?!})
        (WpHole) {NameSet:
        [{Name: System.IO.putStrLn}]}
        (Nothing)))]})]
   [])
  []
  []
  []
  []
  []
  []
  []
  []
  [])
 [
  (L {Implicit import declaration}
   (ImportDecl
    (L {Implicit import declaration} {ModuleName: Prelude})
    (False)
    (False)
    (Nothing)
    (Nothing)))]
 (Nothing)
 (Nothing)
 (HaddockModInfo
  (Nothing)
  (Nothing)
  (Nothing)
  (Nothing)))
------------------------- shown typecheckedSource
{Bag(Located (HsBind Var)):
[
 (L {HelloWorld.hs:2:0-30}
  (AbsBinds
   []
   []
   [
    ((,,,)
     [] {Var: HelloWorld.main} {Var: main}
     [])] {Bag(Located (HsBind Var)):
   [
    (L {HelloWorld.hs:2:0-30}
     (FunBind
      (L {HelloWorld.hs:2:0-3} {Var: main})
      (False)
      (MatchGroup
       [
        (L {HelloWorld.hs:2:0-30}
         (Match
          []
          (Nothing)
          (GRHSs
           [
            (L {HelloWorld.hs:2:7-30}
             (GRHS
              []
              (L {HelloWorld.hs:2:7-30}
               (HsApp
                (L {HelloWorld.hs:2:7-14}
                 (HsVar {Var: System.IO.putStrLn}))
                (L {HelloWorld.hs:2:16-30}
                 (HsLit
                  (HsString {FastString: "Hello, World!"})))))))]
           (EmptyLocalBinds))))] GHC.IOBase.IO ())
      (WpHole) {!NameSet placeholder here!}
      (Nothing)))]}))]}
@
-}
module GHC.SYB.Utils where

import Data.Generics

-- import qualified GHC.Paths
import PprTyThing
import DynFlags
import GHC
import Outputable
import SrcLoc
import qualified OccName(occNameString)
import Bag(Bag,bagToList)
import Var(Var)
import FastString(FastString)
import NameSet(NameSet,nameSetToList)

#if __GLASGOW_HASKELL__ < 700
import GHC.SYB.Instances
#endif 

import Data.List

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

-- | Generic Data-based show, with special cases for GHC Ast types,
--   and simplistic indentation-based layout (the 'Int' parameter); 
--   showing abstract types abstractly and avoiding known potholes 
--   (based on the 'Stage' that generated the Ast)
showData :: Data a => Stage -> Int -> a -> String
showData stage n = 
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan 
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` postTcType `extQ` fixity
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (concat (intersperse " " (gmapQ (showData stage (n+1)) t))) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent n = "\n" ++ replicate n ' ' 
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: FastString -> String
        list l     = indent n ++ "[" 
                              ++ concat (intersperse "," (map (showData stage (n+1)) l)) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDoc . ppr :: Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString 
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc . ppr :: ModuleName -> String
        srcSpan    = ("{"++) . (++"}") . showSDoc . ppr :: SrcSpan -> String
        var        = ("{Var: "++) . (++"}") . showSDoc . ppr :: Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc . ppr :: DataCon -> String

        bagRdrName:: Bag (Located (HsBind RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . bagToList 
        bagName   :: Bag (Located (HsBind Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . bagToList 
        bagVar    :: Bag (Located (HsBind Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . bagToList 

        nameSet | stage `elem` [Parser,TypeChecker] 
                = const ("{!NameSet placeholder here!}") :: NameSet -> String
                | otherwise     
                = ("{NameSet: "++) . (++"}") . list . nameSetToList 

        postTcType | stage<TypeChecker = const "{!type placeholder here?!}" :: PostTcType -> String
                   | otherwise     = showSDoc . ppr :: Type -> String

        fixity | stage<Renamer = const "{!fixity placeholder here?!}" :: GHC.Fixity -> String
               | otherwise     = ("{Fixity: "++) . (++"}") . showSDoc . ppr :: GHC.Fixity -> String

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x 
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- | A variation of 'everything', using a 'GenericQ Bool' to skip
--   parts of the input 'Data'.
--everythingBut :: GenericQ Bool -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
--everythingBut q k z f x 
--  | q x       = z
--  | otherwise = foldl k (f x) (gmapQ (everythingBut q k z f) x)
