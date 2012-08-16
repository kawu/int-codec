import Control.Applicative ((<$>), (<*>))
import Data.Codec

data Tree a b
    = B (Tree a b) a (Tree a b)
    | L b
    deriving (Show, Eq, Ord)

treeCodec :: Codec c a b -> Codec c' a' b'
          -> Codec (c, c') (Tree a a') (Tree b b')
treeCodec nodeCdc leafCdc =
    treeCdc
  where
    treeCdc = Codec (empty nodeCdc, empty leafCdc) enc dec upd

    enc m (B l x r) = B
        <$> encode treeCdc m l
        <*> encode nodeCdc (fst m) x
        <*> encode treeCdc m r
    enc m (L x) = L
        <$> encode leafCdc (snd m) x

    dec m (B l x r) = B
        <$> decode treeCdc m l
        <*> decode nodeCdc (fst m) x
        <*> decode treeCdc m r
    dec m (L x) = L
        <$> decode leafCdc (snd m) x

    upd c0 (B l x r) =
        let (fstC1, x') = update nodeCdc (fst c0) x
            c1 = (fstC1, snd c0)
            (c2, l') = update treeCdc c1 l
            (c3, r') = update treeCdc c2 r
        in  (c3, B l' x' r')
    upd c0 (L x) =
        let (sndC1, x') = update leafCdc (snd c0) x
            c1 = (fst c0, sndC1)
        in  (c1, L x')

tree = B
    (B (L 1) "osioł" (L 2))
    "muflon"
    (B (B (L 3) "kita" (L 4)) "mustang" (L 4))

cdc = treeCodec mapCodec' mapCodec

main = do
    print tree
    print (fromList cdc [tree])
