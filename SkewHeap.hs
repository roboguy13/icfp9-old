module SkewHeap
  where

data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a)

singleton :: a -> SkewHeap a
singleton x = Node x Empty Empty


