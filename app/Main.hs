{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bifunctor
import Data.Unique
import Text.Show.Pretty (pPrint)

import qualified Language.Souffle.Interpreted as S
import qualified Language.Souffle.Marshal as S
import GHC.Generics
import Data.Int
import Data.Foldable
import Control.Monad
import Control.Monad.Reader


data NodeHash = MkHash Int32
  deriving (Show, Generic)

-- Placeholder for source information like SourceSpans.
data SourceInfo = SLOC
  deriving Show

main :: IO ()
main = do
  uprog <- pairWithUnique $ fmap (const SLOC) ex1
  let hashedProg = fmap (second (MkHash . fromIntegral . hashUnique)) uprog :: Program (SourceInfo, NodeHash)
  pPrint hashedProg

  simpMain hashedProg

data Program a = Prog [Statement a]
  deriving (Functor, Show, Traversable, Foldable, Generic)

data Statement a
  = VarDecl Ident (Expr a) a
  | Assignment Ident (Expr a) a
  | FunctionCall Ident (FnArgs a) a
  deriving (Functor, Show, Traversable, Foldable, Generic)

data FnArgs a = FnArgs [Expr a] a
  deriving (Functor, Show, Traversable, Foldable, Generic)

data Expr a
  = StringLit String a
  | Append (Expr a) (Expr a) a
  | Var Ident a
  deriving (Functor, Show, Traversable, Foldable, Generic)

data StringFact = StringFact NodeHash String
  deriving (Show, Generic)

type Ident = String

{-
 - var notSQL = "Quick Brown Fox"
 - var s = "SELECT * FROM FOO";
 - s = s + "WHERE BAR = 3"
 - execQuery(s);
 -}

ex1 :: Program ()
ex1 = Prog [
  VarDecl "notSQL" (StringLit "Quick Brown Fox" ()) ()
 ,VarDecl "s" (StringLit "SELECT * FROM FOO" ()) ()
 ,Assignment "s" (Append (Var "s" ()) (StringLit "WHERE BAR = 3" ()) ()) ()
 ,FunctionCall "execQuery" (FnArgs [Var "s" ()] ()) ()
 ]

decorateWithUnique :: Program a -> IO (Program Unique)
decorateWithUnique = traverse (\_ -> newUnique)

pairWithUnique :: Program a -> IO (Program (a,Unique))
pairWithUnique = traverse $ \a -> do
  u <- newUnique
  return (a, u)

-- Our Datalog Program
data SIMPL = SIMPL

data SQLConst = SQLConst NodeHash
  deriving (Show, Generic)

instance S.Program SIMPL where
  type ProgramFacts SIMPL = '[StringFact, SQLConst]
  programName = const "simpl" --PITFALL: Do not include the .dl extension

instance S.Fact (StringFact) where
  type FactDirection (StringFact) =  'S.Input
  factName = const "lit_string"

instance S.Fact (SQLConst) where
  type FactDirection (SQLConst) = 'S.Output
  factName = const "sqlConst"

instance S.Marshal StringFact
instance S.Marshal NodeHash
instance S.Marshal SQLConst

-- For this simple SQL Const analysis we're only adding string literal constants in variable declarations
-- Traverse the AST and addfacts as we go
deduceFacts :: S.Handle SIMPL -> Program (SourceInfo, NodeHash) -> S.SouffleM ()
deduceFacts handle (Prog xs) = traverse_ (deduceFactsStatement handle) xs

deduceFactsStatement :: S.Handle SIMPL -> Statement (SourceInfo, NodeHash) -> S.SouffleM ()
deduceFactsStatement handle (VarDecl name e _) = deduceFactsExpr handle e
deduceFactsStatement handle _ = return ()

deduceFactsFnArgs :: S.Handle SIMPL -> FnArgs (SourceInfo, NodeHash) -> S.SouffleM ()
deduceFactsFnArgs handle (FnArgs es _) = traverse_ (deduceFactsExpr handle) es

deduceFactsExpr :: S.Handle SIMPL -> Expr (SourceInfo, NodeHash) -> S.SouffleM ()
deduceFactsExpr handle expr@(StringLit s (span, hash)) = do
  liftIO $ putStrLn "Adding :"
  let fact = StringFact hash s
  liftIO $ print fact
  S.addFact handle fact
deduceFactsExpr handle _ = return ()

simpMain :: Program (SourceInfo, NodeHash) -> IO ()
simpMain annotatedProg = S.runSouffle SIMPL $ \case
  Nothing -> liftIO $ putStrLn "Failed to load program, make sure Souffle can find the programName"
  Just progHandle -> do
    liftIO $ putStrLn "SIMPL loaded"
    deduceFacts progHandle annotatedProg
    S.run progHandle
    stringFacts :: [SQLConst] <- S.getFacts progHandle
    liftIO $ do
      putStrLn ""
      putStrLn "Datalog Facts"
      putStrLn "SQL Const Facts: "
      traverse_ print stringFacts
