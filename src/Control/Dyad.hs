{-# LANGUAGE
    UnicodeSyntax
  #-}

module Control.Dyad
  ( Dyad(..)
  , Triad(..)
  , Tetrad(..)
  ) where

import Data.Function.Unicode

import System.IO.Unsafe (unsafePerformIO)

class Dyad γ where
  (>>>=) :: γ (γ α) → (α → γ β) → γ β
  dyad   :: α → γ (γ α)

class Triad γ where
  (>>>>=) :: γ (γ (γ α)) → (α → γ β) → γ β
  triad   :: α → γ (γ (γ α))

class Tetrad γ where
  (>>>>>=) :: γ (γ (γ (γ α))) → (α → γ β) → γ β
  tetrad   :: α → γ (γ (γ (γ α)))

instance Dyad IO where
  dyad = return ∘ return
  x >>>= f = x >>= f ∘ unsafePerformIO

instance Triad IO where
  triad = return ∘ return ∘ return
  x >>>>= f = x >>= f ∘ unsafePerformIO ∘ unsafePerformIO

instance Tetrad IO where
  tetrad = return ∘ return ∘ return ∘ return
  x >>>>>= f = x >>= f ∘ unsafePerformIO ∘ unsafePerformIO ∘ unsafePerformIO

instance Dyad [] where
  dyad x = [[x]]
  m >>>= f = concatMap f (concat m)

instance Triad [] where
  triad x = [[[x]]]
  m >>>>= f = concatMap f (concat $ concat m)

instance Tetrad [] where
  tetrad x = [[[[x]]]]
  m >>>>>= f = concatMap f (concat $ concat $ concat m)

instance Dyad Maybe where
  dyad = Just ∘ Just
  Nothing       >>>= f = Nothing
  Just Nothing  >>>= f = Nothing
  Just (Just x) >>>= f = f x

instance Triad Maybe where
  triad = Just ∘ Just ∘ Just
  Nothing       >>>>= f = Nothing
  Just Nothing  >>>>= f = Nothing
  Just (Just Nothing)  >>>>= f = Nothing
  Just (Just (Just x)) >>>>= f = f x

instance Tetrad Maybe where
  tetrad = Just ∘ Just ∘ Just ∘ Just
  Nothing       >>>>>= f = Nothing
  Just Nothing  >>>>>= f = Nothing
  Just (Just Nothing)  >>>>>= f = Nothing
  Just (Just (Just Nothing))  >>>>>= f = Nothing
  Just (Just (Just (Just x))) >>>>>= f = f x
