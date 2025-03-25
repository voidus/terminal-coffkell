{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Redundant $" #-}
module Foo where

import Input
import Yum

-- $(callEndpoint \$ terminalCoffee.emailSubscribe "void+yo@wthack.de")

-- $(do
--     token <- liftIO $ tokenFromFile "mytoken"
--     callEndpoint $ terminalCoffee.productList token
--  )

{- $(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint $ terminalCoffee.orderCreate token
 )
-}

{- $(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint $ terminalCoffee.cardCollect token
 )
-}

-- $(do
--   token <- liftIO $ tokenFromFile "mytoken"
--   callEndpoint $ terminalCoffee.cardList token
--  )

-- $(do
--   token <- liftIO $ tokenFromFile "mytoken"
--   callEndpoint $ terminalCoffee.addressList token
--  )

{- $( do
    token <- liftIO $ tokenFromFile "mytoken"
    callEndpoint $
      terminalCoffee.addressCreate
        token
        Address
          { name = "Jeff Albertson (Comic Book Guy)"
          , street1 = "123 Nerd Paradise Lane"
          , street2 = "The Android's Dungeon & Baseball Card Shop"
          , city = "Springfield"
          , province = "NT"
          , zip = "12345"
          , country = "US"
          , phone = "555-WORST-EVER"
          }
 )
-}

-- $( do
--     token <- liftIO $ tokenFromFile "mytoken"
--     callEndpoint $
--       terminalCoffee.orderCreate
--         token
--         Order
--           { cardID = "crd_01JQ6ZYMED58NZRA22AQY6RCN7"
--           , addressID = "shp_01JQ71VEYHWJX7CBJNQWA03WS2"
--           , variants = [("prd_01JNH7GKWYRHX45GPRZS3M7A4X", 2)]
--           }
--  )
