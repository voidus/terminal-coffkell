{-# LANGUAGE AllowAmbiguousTypes #-}

module Yum where

import Data.Aeson
import Data.List qualified as List
import GHC.Show (Show (showsPrec))
import GHC.TypeLits (TypeError)
import GHC.TypeLits qualified as TypeLits
import Language.Haskell.TH
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.Client.Generic (genericClient)
import System.Environment (getEnvironment)
import Text.LaTeX
import Prelude hiding (Product, (&))
import System.Process (callProcess)

listProducts :: Q [Dec]
listProducts = callEndpoint terminalCoffee.productList

-- IMPORTANT! We are using NoFieldSelectors to avoid the generation of field NoFieldSelectors
-- When accessing the fields of the data types, we will use the record syntax.

data ProductVariant = ProductVariant
  { id :: Text
  , name :: Text
  , price :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data ProductTags = ProductTags
  { featured :: Bool
  , market_eu :: Bool
  , market_na :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Product = Product
  { id :: Text
  , name :: Text
  , description :: Text
  , variants :: [ProductVariant]
  , tags :: ProductTags
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

instance ToLatex Product where
  inThisPaper = "Analyze the product catalog structure and pricing strategy"

  toLatex p = do
    "We analyze the product " <> fromString (show p.name) <> " with ID " <> fromString (show p.id)
    par
    "Description: " <> fromString (show p.description)
    par
    "Available variants:"
    itemize $
      mconcat $
        p.variants <&> \v ->
          item Nothing
            <> fromString
              ( show
                  ( v.name
                      <> " - Price: $"
                      <> show (v.price `div` 100)
                      <> "."
                      <> show (v.price `mod` 100)
                  )
              )

    par
    "Market availability:"
    itemize $
      mconcat
        [ item Nothing
        , "Featured: " <> if p.tags.featured then "Yes" else "No"
        , item Nothing
        , "Available in NA: " <> if p.tags.market_na then "Yes" else "No"
        , item Nothing
        , "Available in EU: " <> if p.tags.market_eu then "Yes" else "No"
        ]

data Profile
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Address
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Card
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Cart
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Order
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Subscription
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Token
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)
data App
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data InitData
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)
data EmailSubscription
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------
-- JSON
--------------------------------------------------

--------------------------------------------------
-- API
--------------------------------------------------

newtype DataWrapped a = DataWrapped
  { data_ :: a
  }
  deriving stock (Generic, Show)
  deriving newtype (ToLatex)

instance (FromJSON a) => FromJSON (DataWrapped a) where
  parseJSON = withObject "DataWrapped" \o -> DataWrapped <$> o .: "data"

data TerminalCoffeeRoutes route = TerminalCoffeeRoutes
  { -- Product routes
    productList :: route :- "product" :> Get '[JSON] (DataWrapped [Product])
  , productGet :: route :- "product" :> Capture "id" Text :> Get '[JSON] (DataWrapped Product)
  , -- Profile routes
    profileGet :: route :- "profile" :> Get '[JSON] (DataWrapped Profile)
  , profileUpdate :: route :- "profile" :> ReqBody '[JSON] Profile :> Put '[JSON] (DataWrapped Profile)
  , -- Address routes
    addressList :: route :- "address" :> Get '[JSON] (DataWrapped [Address])
  , addressGet :: route :- "address" :> Capture "id" Text :> Get '[JSON] (DataWrapped Address)
  , addressCreate :: route :- "address" :> ReqBody '[JSON] Address :> Post '[JSON] (DataWrapped Address)
  , addressDelete :: route :- "address" :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
  , -- Card routes
    cardList :: route :- "card" :> Get '[JSON] (DataWrapped [Card])
  , cardGet :: route :- "card" :> Capture "id" Text :> Get '[JSON] (DataWrapped Card)
  , cardCreate :: route :- "card" :> ReqBody '[JSON] Card :> Post '[JSON] (DataWrapped Card)
  , cardCollect :: route :- "card" :> "collect" :> Post '[JSON] (DataWrapped Text)
  , cardDelete :: route :- "card" :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
  , -- Cart routes
    cartGet :: route :- "cart" :> Get '[JSON] (DataWrapped Cart)
  , cartAddItem :: route :- "cart" :> "item" :> ReqBody '[JSON] Cart :> Put '[JSON] (DataWrapped Cart)
  , cartSetAddress :: route :- "cart" :> "address" :> ReqBody '[JSON] Text :> Put '[JSON] (DataWrapped Cart)
  , cartSetCard :: route :- "cart" :> "card" :> ReqBody '[JSON] Text :> Put '[JSON] (DataWrapped Cart)
  , cartConvert :: route :- "cart" :> "convert" :> Post '[JSON] (DataWrapped Order)
  , cartClear :: route :- "cart" :> Delete '[JSON] (DataWrapped ())
  , -- Order routes
    orderList :: route :- "order" :> Get '[JSON] (DataWrapped [Order])
  , orderGet :: route :- "order" :> Capture "id" Text :> Get '[JSON] (DataWrapped Order)
  , orderCreate :: route :- "order" :> ReqBody '[JSON] Order :> Post '[JSON] (DataWrapped Order)
  , -- Subscription routes
    subscriptionList :: route :- "subscription" :> Get '[JSON] (DataWrapped [Subscription])
  , subscriptionGet :: route :- "subscription" :> Capture "id" Text :> Get '[JSON] (DataWrapped Subscription)
  , subscriptionCreate :: route :- "subscription" :> ReqBody '[JSON] Subscription :> Post '[JSON] (DataWrapped Subscription)
  , subscriptionCancel :: route :- "subscription" :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
  , -- Token routes
    tokenList :: route :- "token" :> Get '[JSON] (DataWrapped [Token])
  , tokenGet :: route :- "token" :> Capture "id" Text :> Get '[JSON] (DataWrapped Token)
  , tokenCreate :: route :- "token" :> Post '[JSON] Token
  , tokenDelete :: route :- "token" :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
  , -- App routes
    appList :: route :- "app" :> Get '[JSON] (DataWrapped [App])
  , appGet :: route :- "app" :> Capture "id" Text :> Get '[JSON] (DataWrapped App)
  , appCreate :: route :- "app" :> ReqBody '[JSON] App :> Post '[JSON] (DataWrapped App)
  , appDelete :: route :- "app" :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
  , -- Misc routes
    viewInit :: route :- "view" :> "init" :> Get '[JSON] (DataWrapped InitData)
  , emailSubscribe :: route :- "email" :> ReqBody '[JSON] EmailSubscription :> Post '[JSON] (DataWrapped ())
  }
  deriving (Generic)

terminalCoffee :: TerminalCoffeeRoutes (AsClientT ClientM)
terminalCoffee = genericClient

call :: ClientM a -> IO (Either ClientError a)
call endpoint = do
  let baseUrl = BaseUrl Https "api.dev.terminal.shop" 443 ""
  mgr <- newManager tlsManagerSettings
  runClientM endpoint (mkClientEnv mgr baseUrl)

-----------------------------------------------------
-- TemplateHaskell
-----------------------------------------------------

data Whitepaper = Whitepaper

instance
  ( TypeError
      ( TypeLits.Text "======================================"
          TypeLits.:$$: TypeLits.Text "== Oops, I wrote a paper instead D: =="
          TypeLits.:$$: TypeLits.Text "======================================"
          TypeLits.:$$: TypeLits.Text "====== check out Whitepaper.pdf ======"
          TypeLits.:$$: TypeLits.Text "======================================"
      )
  ) =>
  Show Whitepaper
  where
  showsPrec = error "unreachable"

callEndpoint :: (ToLatex a) => ClientM a -> Q [Dec]
callEndpoint doCall = do
  liftIO (List.lookup "COFFEETIME" <$> getEnvironment)
    >>= maybe (pure []) \_ -> do
      response <-
        liftIO (call doCall)
          >>= either (error . show) pure
      liftIO $ writeLatext response
      let name = mkName "yourProgram"
      liftIO $ callProcess "pdflatex" ["-interaction=batchmode", "Whitepaper.tex"]
      pure
        [ SigD name (ConT ''IO `AppT` ConT ''())
        , ValD
            (VarP name)
            (NormalB (AppE (VarE 'print) (ConE 'Whitepaper)))
            []
        ]

-----------------------------------------------------
-- Latex
-----------------------------------------------------

class ToLatex a where
  inThisPaper :: String
  toLatex :: a -> LaTeXT IO ()

instance ToLatex [Product] where
  inThisPaper = "Analyze the complete product catalog, including pricing strategies, market availability, and product distribution across regions"

  toLatex products = do
    section "Product Catalog Analysis"

    -- Overview statistics
    let totalProducts = length products
        featuredCount = length $ filter (.tags.featured) products
        naCount = length $ filter (.tags.market_na) products
        euCount = length $ filter (.tags.market_eu) products

    subsection "Catalog Overview"
    "The product catalog consists of " <> fromString (show totalProducts) <> " products, "
    "with " <> fromString (show featuredCount) <> " featured items. "
    "Market distribution shows " <> fromString (show naCount) <> " products available in North America "
    "and " <> fromString (show euCount) <> " products available in Europe."

    -- Price Analysis
    subsection "Pricing Analysis"
    let allVariants = concatMap ((.variants)) products
        avgPrice = sum (map (.price) allVariants) `div` fromIntegral (length allVariants)
    "The average price point across all variants is $"
    fromString (show $ avgPrice `div` 100)
    "."
    fromString
      (show $ avgPrice `mod` 100)

    -- Individual Product Details
    subsection "Product Details"
    "Below is a detailed analysis of each product in the catalog:"
    par
    mapM_ toLatex products

    -- Market Distribution
    subsection "Market Distribution"
    "Product availability across markets:"
    center $
      tabular Nothing [VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine] $
        do
          hline
          "Region" & "Product Count" & "Percentage" <> lnbk
          hline
          "North America" & fromString (show naCount) & fromString (show (round @Double @Integer (fromIntegral naCount * 100 / fromIntegral totalProducts))) <> "%" <> lnbk
          "European Union" & fromString (show euCount) & fromString (show (round @Double @Integer (fromIntegral euCount * 100 / fromIntegral totalProducts))) <> "%" <> lnbk
          hline

instance ToLatex () where
  inThisPaper = "Do Nothing"
  toLatex () = "henlo"

writeLatext :: forall a. (ToLatex a) => a -> IO ()
writeLatext a = do
  doc <- execLaTeXT do
    documentclass [] article
    author "<TODO fill author>"
    title "Towards a novel source of chemical energy"
    document do
      maketitle
      section "Abstract"
      "This paper presents an implementation and analysis of a client-side coffee ordering system utilizing RESTful API endpoints. The system demonstrates the practical application of HTTP requests to interact with a remote coffee service API, enabling users to place beverage orders programmatically. The implementation showcases standard REST practices, including GET requests to retrieve available menu items and POST requests to submit orders. The paper discusses the handling of API responses, error cases, and the structured JSON payload format used for order specifications, including customization options such as size, temperature, and additional ingredients. Authentication mechanisms and order tracking functionality are also examined. This work provides insights into modern API consumption patterns and serves as a practical example of integrating third-party services into client applications for automated beverage ordering."
      par
      "Keywords: REST API, Coffee Ordering System, HTTP Requests, Client Implementation, API Integration"

      section "Contribution"
      "In this paper, we will:"
      par
      fromString $ inThisPaper @a

      section "Section 1"
      toLatex a

  renderFile "Whitepaper.tex" doc
