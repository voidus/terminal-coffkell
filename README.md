# Terminal Coffkell

Order coffee with haskell!

> The name sounds like a disease
>
> -- A voice inside my head

> [!WARNING]  
> This probably shouldn't exist.

This library allows you to order coffee from terminal.shop using haskell. Well, kinda.
Naturally, it doesn't produce a runnable program, but that doesn't mean you won't order coffee.

I intend this to be usable by people with little experience in Haskell, but if
you stray from the documented path you're pretty much on your own.

Note: If you *really* want to spoil yourself and just find out what this does, check out [the this-is-no-fun branch](https://github.com/voidus/terminal-coffkell/tree/this-is-no-fun)

## Using this library

### The easy way
Note: This way is only easy if you have nix installed.

Further note: There is no need to clone this repository.

1. Create a new directory and `cd` inside it
2. `nix --extra-experimental-features "nix-command flakes" develop github:voidus/terminal-coffkell#order`
3. Create `Whatever.hs` with one of the examples below (the filename doesn't matter)

    Note that library has a safety: the COFFEETIME environment variable needs to
    be defined and non-empty for anything relevant to happen. For your convenience, the nix devshell automatically sets that environment variable. If you want to edit haskell files inside the devshell, you probably want to invoke your editor lik `env COFFEETIME= nvim`.
    
4. Compile it with `ghc Whatever.hs`

### The docker way

Note: This is downloading a lot of stuff into the container. You probably don't want to `--rm` it but re-use the container for your work until you've either finished your Thesis or ordered coffee.

1. Create a new directory and `cd` inside it
2. `docker run -ti --name coffkell -v $PWD:/work -w /work nixos/nix sh`
3. continue with `nix develop ...` from the easy way

- You can later re-use the container by calling `docker start -a coffkell`
- To get rid of it, do `docker rm coffkell`
- If `docker rm` complains about it still running, show no mercy and `docker stop -s 9 coffkell`
- You can probably also run this as not-root somehow

### Other ways

Left as an exercise to the reader.
Note that the library will not do anything unless the COFFEETIME variable is set. This is automatically done

## How to order

Feel free to replace Dev with Prod here if you're feeling it.
But note that I have only proved it compiles, not tried it. (Wrong continent sorry)

0. Subscribe to the mailing list (note that this also seems to work on the dev server maybe)

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$(callEndpoint Dev $ terminalCoffee.emailSubscribe "your@email.com")
```


1. Check out the products:

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$(callEndpoint Dev terminalCoffee.productList)
```

2. Get a token via `ssh dev.terminal.shop -t tokens` and put it in a file.
   I recomment `mytoken`. If you choose a different name, adjust the filename
    in the following examples

3. Make sure you have a card on file:

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint Dev $ terminalCoffee.cardList token
 )
```

Make sure to note down the ID for later.

If you don't have one yet, you can generate a link to add one:

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint Dev $ terminalCoffee.cardCollect token
 )
```

4. Make sure you have a delivery address on file:

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$(do
  token <- liftIO $ tokenFromFile "mytoken"
  callEndpoint Dev $ terminalCoffee.addressList token
 )
```

Again, make sure to note down the ID for later.

If you don't have one yet, you can add an address like so:

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$( do
    token <- liftIO $ tokenFromFile "mytoken"
    callEndpoint Dev $
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
{-# LANGUAGE TemplateHaskell, OverloadedRecordDot, DisambiguateRecordFields, OverloadedStrings #-}
import Yum
import Control.Monad.IO.Class
import Data.Text

$( do
    token <- liftIO $ tokenFromFile "mytoken"
    callEndpoint Dev $
      terminalCoffee.orderCreate
        token
        Order
          { cardID = "crd_XXXXXXXXXXXXXXXXXXXXXXXXXX"
          , addressID = "shp_XXXXXXXXXXXXXXXXXXXXXXXXXX"
          , variants = [("prd_01JNH7GKWYRHX45GPRZS3M7A4X", 2)]
          }
 )
```

## FAQ

#### Why are you doing this?
Honestly, same.

#### Will this delete my harddrive?
Maybe. It shouldn't, but see the note at the top.

#### Can I actually order coffee with this?
I hope? It works against the dev environment. As I wrote, I can't order since I'm in EU.
If you successfully ordered through this, please let me know!

#### Is this AI slop?
Of course. It's a paper produced in 2025 (or whatever year it is).

That said, the readme was exclusively written by me without AI assistance.
