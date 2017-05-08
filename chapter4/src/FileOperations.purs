module FileOperations where

import Prelude

import Data.Path (Path, ls, isDirectory, size, root, filename)
import Data.Array (concatMap, (:))
import Data.Foldable (foldl)
import Data.Array.Partial (head)
import Partial.Unsafe (unsafePartial)
import Data.Maybe

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Excersises

onlyFiles :: Path -> Array Path
onlyFiles root =
  if isDirectory root
  then concatMap onlyFiles (ls root)
  else [root]

largestFiles :: Array Path
largestFiles =
  foldl (\b a -> if (size a > size (unsafePartial head b))
                 then [a]
                 else if (size a == size (unsafePartial head b))
                      then a : b
                      else b
        ) [] (onlyFiles root)

smallestFiles :: Array Path
smallestFiles =
  foldl (\b a -> if (size a < size (unsafePartial head b))
                 then [a]
                 else if (size a == size (unsafePartial head b))
                      then a : b
                      else b
        ) largestFiles (onlyFiles root)
  
whereIs' :: String -> Path -> Array (Maybe Path)
whereIs' name dir = do
  child <- ls dir
  if (isDirectory child)
    then whereIs' name child
    else if (filename child == name)
         then [Just dir]
         else [Nothing]

whereIs :: String -> Maybe Path
whereIs name = foldl (\b a -> if isJust a then a else b)
               Nothing
               (whereIs' name root)
               
  
