module XvsO.Utils
  ( setAt

  , (<$$>)
  , (...)
  ) where

infixl 4 <$$>
(<$$>)
  :: (Functor wrapper, Functor inner)
  => (a -> b) -> wrapper (inner a) -> wrapper (inner b)
f <$$> m = (f <$>) <$> m

infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \a b -> f $ g a b

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []