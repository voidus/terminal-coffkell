# How to order

TODO: Production server

IMPORTANT: All of these have to be compiled with `env COFFEETIME=yes ghc File.hs` to work.

0. Subscribe to the mailing list (note that this also seems to work on the dev server maybe)

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Yum

$(callEndpoint $ terminalCoffee.emailSubscribe "your@email.com")
```

1. Check out the products:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Yum
$(listProducts)
```

2. Get a token via `ssh dev.terminal.coffee -t tokens` and put it in a file. I recomment `mytoken`.

3. Make sure you have a card on file:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Yum

$(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint $ terminalCoffee.cardList token
 )
```

Make sure to note down the ID for later.

If you don't have one yet, you can generate a link to add one:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Yum

$(do --   token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint $ terminalCoffee.cardCollect token
 )
```

4. Make sure you have a delivery address on file:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Yum

$(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint $ terminalCoffee.addressList token
 )
```

Again, make sure to note down the ID for later.

If you don't have one yet, you can add an address like so:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Yum

$( do
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
```


5. Not you're ready to order:

```haskell
{-# LANGUAGE TemplateHaskell #-}
