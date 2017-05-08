module Ex01 where

import Prelude

import Data.String (toCharArray, stripPrefix, Pattern(..))

-- 11.4
import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

-- 11.5
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Data.Traversable (sequence)
import Data.Foldable (foldr)

-- 11.6
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Monoid.Additive

-- 11.9
import Control.Monad.Except.Trans
import Data.Identity
import Data.Maybe (Maybe(..))
import Data.Array (foldM)

-- 11.13
import Control.Alternative
import Data.List

----------------------------------------

-- 11.4

testParens :: String -> Boolean
testParens s0 = (ucparens s0) == 0 where
  ucparens s = execState (
    do
      traverse_ (\c -> case c of
                    '(' -> modify (\st -> st + 1)
                    ')' -> modify (\st -> st - 1)
                    _ -> pure unit
                ) (toCharArray s)
    ) 0

-- 11.5

type Level = Int
type Doc = Reader Level String

indentSP :: Int -> String
indentSP 0 = ""
indentSP n = "\t" <> indentSP (n-1)

line :: String -> Doc
line s = do
  l <- ask
  pure $ (indentSP l) <> s
  
indent :: Doc -> Doc
indent = local (_ + 1)

cat :: Array Doc -> Doc
cat ds = map (\ss -> foldr (\a b -> a <> "\n" <> b) "" ss) (sequence ds)

render :: Doc -> String
render ds = runReader ds 0

testd :: Doc
testd = cat [ line "Here is some indented text:"
            , indent $ cat
              [ line "I am indented"
              , line "So am I"
              , indent $ line "I am even more indented"
              ]
            ]

-- 11.6
{-
sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n
-}

sumArray :: Array Int -> Writer (Additive Int) Unit
sumArray = traverse_ \n -> tell (Additive n)

collatz :: Int -> Int
collatz n = cltz n 1 where
  cltz 1 a = a
  cltz n' a = if n' `mod` 2 == 0
              then cltz (n' / 2) (a + 1)
              else cltz (3 * n' + 1) (a + 1)

collatzLog :: Int -> Writer (Array String) Int
collatzLog n = cltz n 1 where
  cltz :: Int -> Int -> Writer (Array String) Int
  cltz 1 a = do
    tell ["1"]
    pure a
  cltz n' a = do
    tell [show n' <> ","]
    if n' `mod` 2 == 0
      then cltz (n' / 2) (a + 1)
      else cltz (3 * n' + 1) (a + 1)

-- 11.9

safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide x y = do
  if y == 0.0
    then throwError "divition by zero"
    else pure (x / y)

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runIdentity :: forall a. Identity a -> a
runIdentity (Identity a) = a

runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

string :: String -> Parser String
string str = do
  stat <- get
  tell ["The state is "<>stat]
--  lift $ tell ["The state is "<>stat]
  case stripPrefix (Pattern str) stat of
    Just rst -> do
      put rst
      pure str
--    Nothing -> lift $ lift $ throwError ["no match"]
    Nothing -> throwError ["no match"]
    
type Level' = Int
type Doc' = ReaderT Level' (WriterT (Array String) Identity) Unit

line' :: String -> Doc'
line' s = do
  level <- ask
  lift $ tell [(indentSP level) <> s]

indent' :: Doc' -> Doc'
indent' = local (_ + 1)

render' :: Doc' -> String
render' ds = foldr (\a b -> a <> "\n" <> b) "" (runIdentity (execWriterT (runReaderT ds 0)))

-- 11.13

pAsThenBs :: Parser Unit
pAsThenBs = do
  _ <- many (string "a")
  _ <- many (string "b")
  pure unit

pAsOrBs :: Parser (List (List String))
pAsOrBs = many (some (string "a") <|> some (string "b"))

type ParserR = ExceptT Errors (StateT String (WriterT Log Identity))

runParserR p s = runIdentity $ runWriterT $ runStateT (runExceptT p) s

stringR :: String -> ParserR String
stringR str = do
  stat <- get
  tell ["The state is "<>stat]
--  lift $ tell ["The state is "<>stat]
  case stripPrefix (Pattern str) stat of
    Just rst -> do
      put rst
      pure str
--    Nothing -> lift $ lift $ throwError ["no match"]
    Nothing -> throwError ["no match"]

{-
pAsThenBsR :: ParserR Unit
pAsThenBsR = do
  _ <- many (stringR "a")
  _ <- many (stringR "b")
  pure unit
-}
