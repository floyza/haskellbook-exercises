module Sandbox where

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello v) == (Hello v') = v == v'
  (Goodbye v) == (Goodbye v') = v == v'
  _ == _ = False
