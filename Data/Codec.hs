{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Codec
( Codec (..)
, fromList
, mapCodec
, mapCodec'
, travCodec
, listCodec
) where

import Prelude hiding (mapM)
import Data.List (foldl')
import Data.Traversable (Traversable, mapM, mapAccumL)
import qualified Data.Map as M

-- | Codec structure for a particular 'c', 'a' and 'b' parameters.
-- It describes the behaviour of a codec which transforms an 'a' to
-- a 'b' structure, while codec itself is represented by the 'c'
-- parameter.
data Codec c a b = Codec
    { empty     :: c
    , encode    :: c -> a -> Maybe b
    , decode    :: c -> b -> Maybe a
    , update    :: c -> a -> (c, b) }

update_ :: Codec c a b -> c -> a -> c
update_ cdc m = fst . update cdc m

fromList :: Codec c a b -> [a] -> c
fromList cdc = foldl' (update_ cdc) (empty cdc)

updateMap :: Ord a => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k  -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp

mapCodec :: Ord a => Codec (M.Map a Int, M.Map Int a) a Int
mapCodec =
    Codec (M.empty, M.empty) enc dec upd
  where
    enc (m, _) x = M.lookup x m
    dec (_, r) y = M.lookup y r
    upd (m, r) x = 
        let m' = updateMap m x
            y  = m' M.! x
            r' = M.insert y x r
        in  m' `seq` r' `seq` ((m', r'), y)

-- | One way Data.Map codec -- when we don't care about decoding. 
mapCodec' :: Ord a => Codec (M.Map a Int) a Int
mapCodec' =
    Codec M.empty enc dec upd
  where
    enc m x = M.lookup x m
    dec _ y = Nothing
    upd m x = 
        let m' = updateMap m x
            y  = m' M.! x
        in  m' `seq` y `seq` (m', y)

travCodec :: Traversable t => Codec c a b -> Codec c (t a) (t b)
travCodec elemCdc =
    Codec (empty elemCdc) enc dec upd
  where
    enc m = mapM (encode elemCdc m)
    dec m = mapM (decode elemCdc m)
    upd m = mapAccumL (update elemCdc) m

listCodec :: Codec c a b -> Codec c [a] [b]
listCodec = travCodec
