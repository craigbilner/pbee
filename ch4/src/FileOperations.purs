module FileOperations where

import Prelude

import Data.Path (Path, ls, isDirectory, size, root, filename)
import Data.Array (concatMap, (:), filter)
import Data.Traversable (foldr)
import Data.Maybe
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not <<< isDirectory) $ allFiles' path

onlyFiles' :: Path -> Array Path
onlyFiles' path =
  let
    rest = do
           child <- ls path
           onlyFiles' child
  in
    if isDirectory path
    then rest
    else path : rest

newtype MinMaxFiles = MinMaxFiles
  {
    largest  :: Path
  , smallest :: Path
  }

instance showMinMaxFiles :: Show MinMaxFiles where
  show (MinMaxFiles { largest: l, smallest: s }) = show [show l, show s]

minMaxFileSize :: Path -> Maybe MinMaxFiles
minMaxFileSize p = foldr minMax Nothing $ onlyFiles' p
  where
    minMax x xs = case xs of
                  Nothing -> Just $ MinMaxFiles { largest: x, smallest: x }
                  Just mm -> case size x of
                             Nothing -> Just mm
                             Just s  -> Just $ updateLarger x s $ updateSmaller x s mm
    updateLarger file fileSize (MinMaxFiles mm) = case size mm.largest of
                                                  Nothing -> MinMaxFiles $ mm { largest = file }
                                                  Just s  -> if fileSize > s
                                                             then MinMaxFiles $ mm { largest = file }
                                                             else MinMaxFiles mm
    updateSmaller file fileSize (MinMaxFiles mm) = case size mm.smallest of
                                                   Nothing -> MinMaxFiles $ mm { smallest = file }
                                                   Just s  -> if fileSize < s
                                                              then MinMaxFiles $ mm { smallest = file }
                                                              else MinMaxFiles mm
isFile :: Path -> Boolean
isFile = not <<< isDirectory

whereIs :: String -> Maybe Path
whereIs s = getDir $ findFile root
  where
    getDir [f] = Just f
    getDir _   = Nothing
    findFile path = do
      child <- ls path
      if isFile child && (filename child == s)
        then [path]
        else findFile child
