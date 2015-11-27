 - Monad
 - Dyad
 - Triad
 - Tetrad

``` haskell
class Dyad γ where
  (>>>=) :: γ (γ α) → (α → γ β) → γ β
  dyad   :: α → γ (γ α)

instance Dyad IO where
  dyad = return ∘ return
  x >>>= f = x >>= f ∘ unsafePerformIO

instance Dyad [] where
  dyad x = [[x]]
  m >>>= f = concatMap f (concat m)

instance Dyad Maybe where
  dyad = Just ∘ Just
  Nothing       >>>= f = Nothing
  Just Nothing  >>>= f = Nothing
  Just (Just x) >>>= f = f x
```

Usage...
--------

``` haskell
mb :: Maybe (Maybe (Maybe Int))
mb = Just $ dyad 1

umb :: Maybe Int
umb = mb >>>= \x -> x

main :: IO ()
main = let a = dyad umb
       in a >>>= print
```
