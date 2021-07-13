module AlgebraicDatatypes.Newtype where

newtype Goats = Goats Int deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Goats where
  tooMany (Goats n) = n > 43
