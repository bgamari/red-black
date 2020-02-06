{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module RBTree
  ( RBTree(..)
  , insert
  , renderRBTree
  , pathInvariant
  , colorInvariant
  ) where

import Data.Monoid
import Diagrams hiding (Color)
import qualified Diagrams.Prelude as D
import Diagrams.Backend.Html5.CmdLine hiding (B)
import Diagrams.TwoD.Layout.Tree
import Data.Default
import Physics.ForceLayout
import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

data Color = R | B
  deriving (Show, Eq, Ord)

toColour :: Color -> D.Colour Double
toColour R = D.red
toColour B = D.black

data RBTree a
  = RBNode Color (RBTree a) a (RBTree a)
  | RBLeaf
  deriving (Show, Functor)

toTree :: RBTree a -> T.Tree (Color, Maybe a)
toTree RBLeaf = T.Node (B, Nothing) []
toTree (RBNode c l x r) = T.Node (c, Just x) [toTree l, toTree r]

colorInvariant :: RBTree a -> Bool 
colorInvariant RBLeaf = True
colorInvariant (RBNode R l _ r) =
     rbColor l == B
  && rbColor r == B
  && colorInvariant l
  && colorInvariant r

pathsToLeaves :: RBTree a -> [[RBTree a]]
pathsToLeaves x@RBLeaf = [[RBLeaf]]
pathsToLeaves x@(RBNode _ l _ r) = map (x:) (pathsToLeaves l ++ pathsToLeaves r)

pathInvariant :: RBTree a -> Bool
pathInvariant t =
    all (== head blackDistances) blackDistances
  where
    blackDistances = map blackDistance (pathsToLeaves t)
    blackDistance = length . filter (\n -> rbColor n == B)

balance :: RBTree a -> RBTree a
balance (RBNode c l x y) = balance' c l x y 
balance RBLeaf = RBLeaf

balance' :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance' B (RBNode R (RBNode R a x b) y c) z d = RBNode R (RBNode B a x b) y (RBNode B c z d)
balance' B (RBNode R a x (RBNode R b y c)) z d = RBNode R (RBNode B a x b) y (RBNode B c z d)
balance' B a x (RBNode R (RBNode R b y c) z d) = RBNode R (RBNode B a x b) y (RBNode B c z d)
balance' B a x (RBNode R b y (RBNode R c z d)) = RBNode R (RBNode B a x b) y (RBNode B c z d)
balance' color a x b = RBNode color a x b

rbColor :: RBTree a -> Color
rbColor (RBNode c _ _ _) = c
rbColor RBLeaf = B

blacken :: RBTree a -> RBTree a
blacken (RBNode _ l x y) = RBNode B l x y
blacken RBLeaf = RBLeaf

insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = blacken $ insert' x t

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' x RBLeaf = RBNode R RBLeaf x RBLeaf
insert' x (RBNode c l y r)
  | x <  y = balance $ RBNode c (insert' x l) y r
  | x >  y = balance $ RBNode c l y (insert' x r)
  | otherwise = RBNode c l x r

testTree :: RBTree String
testTree = fmap show $ foldr insert RBLeaf [41, 38, 31, 12, 19, 8, 42] 

main :: IO ()
main = do
  print testTree
  defaultMain $ renderRBTree testTree

renderRBTree :: RBTree String -> QDiagram Html5 V2 Double Any
renderRBTree rbTree = 
    frame 1 $ scale 60 $ renderTree renderNode renderEdge graph'
  where
    graph' = symmLayout' slOpts $ toTree rbTree
    slOpts = slHSep .~ 3 $ slVSep .~ 2.5 $ def

renderNode :: (Color, Maybe String)
           -> QDiagram Html5 V2 Double Any
renderNode (c, Just lbl) = frame 1 $ (text lbl & fontSizeL 0.8) `atop` (circle 1 & lc (toColour c) & fc D.white)
renderNode (c, Nothing)  = frame 1 $ circle 0.5 & lc (toColour c) & fc D.white

renderEdge = (~~)
