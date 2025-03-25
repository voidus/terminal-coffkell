{-# OPTIONS_GHC -Wno-unused-imports #-}
module Foo where

import Input
import Yum

$(callEndpoint $ terminalCoffee.emailSubscribe $ EmailSubscription "void@wthack.de")
