module XvsO.Utils
  ( (<$$>)
  ) where

infixl 4 <$$>
(<$$>)
  :: (Functor wrapper, Functor inner)
  => (a -> b) -> wrapper (inner a) -> wrapper (inner b)
f <$$> m = (f <$>) <$> m

