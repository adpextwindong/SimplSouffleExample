# SIMPL Souffle-Haskell Example

## Building and Running

```
nix-shell
cabal run
```

## Overview

This repo is a [`souffle-haskell`](https://github.com/luc-tielen/souffle-haskell) minimal example to showcase emitting facts from an AST and running analysis in Souffle Datalog.

Read [Static analysis using Haskell and Datalog](https://luctielen.com/posts/static_analysis_using_haskell_and_datalog/) by Luc if you haven't already.

The [simpl.dl](simpl.dl) Datalog we're looking to run is:

```datalog
//Input facts

//Conventions: Node Id's are unsigned
//             Identifiers are symbols
//             Every AST node has a node_id

//Expr
.decl lit_string(node_id: unsigned, value: symbol)

.decl sqlConst(node: unsigned)

.input lit_string
.output sqlConst

sqlConst(node) :-
  lit_string(node, value),
  contains("SELECT", value).
```

We'd like to check if string constants in our program start with "SELECT".

The example program we're going to analyse is this:

```
var notSQL = "Quick Brown Fox"
var s = "SELECT * FROM FOO";
s = s + "WHERE BAR = 3"
execQuery(s);
```

The AST for this simple toy language looks like this:

```haskell
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
```

`Data.Unique` in the IO monad is used to decorate every AST with a unique hash.

We deduce facts from the AST in the `S.SouffleM` monad.

Whenever we see a string literal in a variable declaration expression we emit it using this datatype.

```haskell
data StringFact = StringFact NodeHash String
  deriving (Show, Generic)
```

Here's the AST traversal.

```haskell
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
```

The boilerplate needed for `souffle-haskell` to interopt with `simpl.dl`

```haskell
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
```

And finally the analyse main with our annotated program interacting with souffle.

```haskell
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
```

### Note

`souffle-haskell` has a Marshal typeclass that leverages GHC.Generics to marshal data to and from Datalog. It can figure out how to derive marshaling for product types but not sum types at the moment. In our example we use a seperate `StringFact` type seperate from `Expr` to get around this. This type must be laid out in the same order as your datalog declaration:

```haskell
data NodeHash = MkHash Int32
  deriving (Show, Generic)

data StringFact = StringFact NodeHash String
  deriving (Show, Generic)
```

```datalog
.decl lit_string(node_id: unsigned, value: symbol)
```

Another thing to take care about is the program name in your Souffle Program instance. If Souffle fails to load double check this.
