{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import qualified Data.Text.Lazy as TL

data Item = Item { itemId :: String, desc :: String, pricePerUnit :: Float, quantity :: Int} deriving (Show, Generic)
type DiscountedItems = [String]
data Cart = Cart {
  discount :: Float,
  discountedItemIds :: DiscountedItems,
  cart :: [Item]
} deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item
instance ToJSON Cart
instance FromJSON Cart

calc_total :: [Item] -> Float
calc_total [item] = pricePerUnit item * fromIntegral(quantity item)
calc_total (item:items) = (pricePerUnit item * fromIntegral(quantity item)) + calc_total items

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    post "/cart" $ do
      shoppingCart <- jsonData
      let items = cart shoppingCart
      let isDiscounted = any (\x -> elem (itemId x) (discountedItemIds shoppingCart)) items
      let undiscountedTotal = calc_total(items)
      let discountedTotal = if isDiscounted
            then undiscountedTotal * discount shoppingCart
            else undiscountedTotal
      json (discountedTotal)
                                                            

