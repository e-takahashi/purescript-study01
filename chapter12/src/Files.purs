module Files where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, Fn3, runFn4, runFn3)
import Types (Async)

--
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Traversable (traverse)
import Data.Foldable (foldr)

import Control.Parallel.Class

foreign import data FS :: Effect

type ErrorCode = String

type FilePath = String

foreign import readFileImpl ::
                 forall eff. Fn3 FilePath
                   (String -> Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

foreign import writeFileImpl ::
                 forall eff. Fn4 FilePath
                   String
                   (Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

readFile :: forall eff. FilePath -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: forall eff. FilePath -> String -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: forall eff. FilePath -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: forall eff. FilePath -> String -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

readFileContEx :: forall eff. FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: forall eff. FilePath -> String -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: forall eff. FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content

-- 12.4
concatFileCont :: forall eff
                  . FilePath
                  -> FilePath
                  -> FilePath
                  -> Async (fs :: FS | eff) (Either ErrorCode Unit)
concatFileCont src1 src2 dest = do
  e1 <- readFileCont src1
  case e1 of
    Left err1 -> pure $ Left err1
    Right cont1 -> do
      e2 <- readFileCont src2
      case e2 of
        Left err2 -> pure $ Left err2
        Right cont2 -> writeFileCont dest (cont1 <> cont2)

-- 12.5
concatFileContEx :: forall eff. FilePath -> FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatFileContEx src1 src2 dest = do
  cont1 <- readFileContEx src1
  cont2 <- readFileContEx src2
  writeFileContEx dest (cont1 <> cont2)

concatenateMany :: forall eff. (Array FilePath) -> FilePath -> ExceptT ErrorCode (Async (fs::FS|eff)) Unit
concatenateMany srcs dest = do
  contents <- traverse (\src -> readFileContEx src) srcs
  let content = foldr (<>) "" contents
  writeFileContEx dest content
  
-- 12.7 3
concatenateManyP :: forall eff. (Array FilePath) -> FilePath -> ExceptT ErrorCode (Async (fs::FS|eff)) Unit
concatenateManyP srcs dest = do
  contents <- sequential $ traverse (\src -> parallel (readFileContEx src)) srcs
  let content = foldr (<>) "" contents
  writeFileContEx dest content
