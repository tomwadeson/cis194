module JoinList where

import Data.Monoid ((<>))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(++++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(++++) x y = Append (tag x <> tag y) x y
