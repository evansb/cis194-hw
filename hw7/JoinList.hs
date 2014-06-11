
module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m0 _) = m0
tag (Append m0 _ _) = m0
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl0 jl1 = Append (mappend (tag jl0) (tag jl1)) jl0 jl1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                      = Nothing
indexJ i0 (Single _ a0)             = if i0 == 0 then Just a0 else Nothing
indexJ i0 (Append m0 jl0 jl1)
    | i0 < 0 || i0 > centerSize     = Nothing
    | i0 < leftSize                 = indexJ (leftSize - i0) jl0
    | otherwise                     = indexJ (i0 - leftSize) jl1
    where
        leftSize = getSize $ size $ tag jl0
        centerSize = getSize $ size m0

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty               = Empty
dropJ 1 (Single _ _)        = Empty
dropJ i0 jl0 | i0 < 1       = jl0
dropJ i0 (Append m0 jl0 jl1)
    | i0 == centerSize      = Empty
    | i0 <= leftSize        = dropJ i0 jl0 +++ jl1
    | i0 > leftSize         = dropJ (i0 - leftSize) jl1
    where
        leftSize = getSize $ size $ tag jl0
        centerSize = getSize $ size m0
dropJ _ _ = undefined

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty               = Empty
takeJ 1 jl0@(Single _ _)    = jl0
takeJ i0 _ | i0 < 1         = Empty
takeJ i0 (Append m0 jl0 jl1)
    | i0 == centerSize      = jl0 +++ jl1
    | i0 <= leftSize        = takeJ i0 jl0
    | i0 > leftSize         = jl0 +++ takeJ (i0 - leftSize) jl1
    where
        leftSize = getSize $ size $ tag jl0
        centerSize = getSize $ size m0
takeJ _ _ = undefined

---- Too lazy, this is all I think
