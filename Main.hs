{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Diagrams hiding (Color)
import Data.Default
import Web.Scotty
import RBTree
import Diagrams.Backend.Html5
import Diagrams.Core.Compile
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB


treeFromList :: Ord a => [a] -> RBTree a
treeFromList = foldr insert RBLeaf

main :: IO ()
main = do
  scotty 3000 $ do
    get "/" $ do
      xs <- param "xs"
      let xs' :: [Int]
          xs' = map (read . T.unpack) $ T.splitOn "," $ T.pack xs
          tree = fmap show $ treeFromList xs'
          dia = renderRBTree tree
      html $ TB.toLazyText $ renderDia Html5 opts dia
  where
    opts :: Diagrams.Backend.Html5.Options Html5 V2 Double
    opts = Html5Options (mkWidth 750) True "canvas"

