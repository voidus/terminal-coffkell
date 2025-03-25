{-# LANGUAGE AllowAmbiguousTypes #-}

module Yum where

import Analytics (invasivelyGatherPII)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import GHC.Show (Show (showsPrec))
import GHC.TypeLits (TypeError)
import GHC.TypeLits qualified as TypeLits
import Language.Haskell.TH
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Auth qualified as Servant
import Servant.Auth.Client qualified as Auth
import Servant.Client
import Servant.Client.Generic (genericClient)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Process (callProcess)
import Text.Groom (groom)
import Text.LaTeX hiding (appendix)
import Text.LaTeX qualified as LaTeX
import Text.LaTeX.Base.Class (fromLaTeX)
import Text.LaTeX.Base.Syntax (LaTeX (TeXEnv), TeXArg (OptArg))
import Prelude hiding (Product, (&))

-- listProducts :: Q [Dec]
-- listProducts = callEndpoint terminalCoffee.productList

order :: Auth.Token -> OrderInput -> Q [Dec]
order token OrderInput{..} = do
  cardID <- do
    let path = toString cardIDFilename
    liftIO (doesFileExist path) >>= \case
      True -> decodeUtf8 <$> readFileBS path
      False -> error $ "Token file \"" <> toText path <> "\" not found"
  callEndpoint $ terminalCoffee.orderCreate token Order{..}

data OrderInput = OrderInput
  { cardIDFilename :: Text
  , addressID :: Text
  , variants :: [(Text, Int)]
  }

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

data Profile
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data AddressWithId = AddressWithId
  { id :: Text
  , name :: Text
  , street1 :: Text
  , street2 :: Text
  , city :: Text
  , province :: Text
  , zip :: Text
  , country :: Text
  , phone :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)
data Address = Address
  { name :: Text
  , street1 :: Text
  , street2 :: Text
  , city :: Text
  , province :: Text
  , zip :: Text
  , country :: Text
  , phone :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)
data Card = Card
  { id :: Text
  , brand :: Text
  , expiration :: CardExpiration
  , last4 :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data CardExpiration = CardExpiration
  { month :: Int
  , year :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Cart
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
data Order = Order
  { cardID :: Text
  , addressID :: Text
  , variants :: [(Text, Int)]
  }
  deriving stock (Generic, Show)
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
newtype EmailSubscription = EmailSubscription
  { email :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString)
newtype OrderCreated = OrderCreated Text
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------
-- JSON
--------------------------------------------------

instance (FromJSON a) => FromJSON (DataWrapped a) where
  parseJSON = withObject "DataWrapped" \o -> DataWrapped <$> o .: "data"

instance (ToJSON Order) where
  toJSON o =
    object
      [ "cardID" .= o.cardID
      , "addressID" .= o.addressID
      , "variants"
          .= object
            [ fromText variant .= count
            | (variant, count) <- o.variants
            ]
      ]

--------------------------------------------------
-- API
--------------------------------------------------

newtype DataWrapped a = DataWrapped
  { data_ :: a
  }
  deriving stock (Generic, Show)
  deriving newtype (ToLatex)

data EmailSubscribeOk = SimplyOk
  deriving stock (Generic, Show)

instance FromJSON EmailSubscribeOk where
  parseJSON = withText "SimplyOk" (const $ pure SimplyOk)

tokenFromFile :: (MonadIO m) => FilePath -> m Auth.Token
tokenFromFile path = do
  liftIO (doesFileExist path) >>= \case
    True -> Auth.Token . BS.strip <$> readFileBS path
    False -> error $ "Token file \"" <> toText path <> "\" not found"

type BearerAuth = Servant.Auth '[Auth.Bearer] ()

newtype CardCollectLink = CardCollectLink {url :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype AddressID = AddressID Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON)

data TerminalCoffeeRoutes route = TerminalCoffeeRoutes
  { -- Product routes
    productList :: route :- "product" :> BearerAuth :> Get '[JSON] (DataWrapped [Product])
  , productGet :: route :- "product" :> Capture "id" Text :> Get '[JSON] (DataWrapped Product)
  , -- Profile routes
    --   profileGet :: route :- "profile" :> BearerAuth :> Get '[JSON] (DataWrapped Profile)
    -- , profileUpdate :: route :- "profile" :> BearerAuth :> ReqBody '[JSON] Profile :> Put '[JSON] (DataWrapped Profile)
    -- Address routes
    addressList :: route :- "address" :> BearerAuth :> Get '[JSON] (DataWrapped [AddressWithId])
  , -- , addressGet :: route :- "address" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped Address)
    addressCreate :: route :- "address" :> BearerAuth :> ReqBody '[JSON] Address :> Post '[JSON] (DataWrapped AddressID)
  , -- , addressDelete :: route :- "address" :> BearerAuth :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
    -- Card routes
    cardList :: route :- "card" :> BearerAuth :> Get '[JSON] (DataWrapped [Card])
  , -- , cardGet :: route :- "card" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped Card)
    -- , cardCreate :: route :- "card" :> BearerAuth :> ReqBody '[JSON] Card :> Post '[JSON] (DataWrapped Card)
    cardCollect :: route :- "card" :> "collect" :> BearerAuth :> Post '[JSON] (DataWrapped CardCollectLink)
  , -- , cardDelete :: route :- "card" :> BearerAuth :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
    -- Cart routes
    --   cartGet :: route :- "cart" :> BearerAuth :> Get '[JSON] (DataWrapped Cart)
    -- , cartAddItem :: route :- "cart" :> "item" :> BearerAuth :> ReqBody '[JSON] Cart :> Put '[JSON] (DataWrapped Cart)
    -- , cartSetAddress :: route :- "cart" :> "address" :> BearerAuth :> ReqBody '[JSON] Text :> Put '[JSON] (DataWrapped Cart)
    -- , cartSetCard :: route :- "cart" :> "card" :> BearerAuth :> ReqBody '[JSON] Text :> Put '[JSON] (DataWrapped Cart)
    -- , cartConvert :: route :- "cart" :> "convert" :> BearerAuth :> Post '[JSON] (DataWrapped Void)
    -- , cartClear :: route :- "cart" :> BearerAuth :> Delete '[JSON] (DataWrapped ())
    -- Order routes
    --   orderList :: route :- "order" :> BearerAuth :> Get '[JSON] (DataWrapped [Void])
    -- , orderGet :: route :- "order" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped Void)
    orderCreate :: route :- "order" :> BearerAuth :> ReqBody '[JSON] Order :> Post '[JSON] (DataWrapped OrderCreated)
  , -- Subscription routes
    --   subscriptionList :: route :- "subscription" :> BearerAuth :> Get '[JSON] (DataWrapped [Subscription])
    -- , subscriptionGet :: route :- "subscription" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped Subscription)
    -- , subscriptionCreate :: route :- "subscription" :> BearerAuth :> ReqBody '[JSON] Subscription :> Post '[JSON] (DataWrapped Subscription)
    -- , subscriptionCancel :: route :- "subscription" :> BearerAuth :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
    -- Token routes
    --   tokenList :: route :- "token" :> BearerAuth :> Get '[JSON] (DataWrapped [Token])
    -- , tokenGet :: route :- "token" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped Token)
    -- , tokenCreate :: route :- "token" :> BearerAuth :> Post '[JSON] Token
    -- , tokenDelete :: route :- "token" :> BearerAuth :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
    -- App routes
    --   appList :: route :- "app" :> BearerAuth :> Get '[JSON] (DataWrapped [App])
    -- , appGet :: route :- "app" :> BearerAuth :> Capture "id" Text :> Get '[JSON] (DataWrapped App)
    -- , appCreate :: route :- "app" :> BearerAuth :> ReqBody '[JSON] App :> Post '[JSON] (DataWrapped App)
    -- , appDelete :: route :- "app" :> BearerAuth :> Capture "id" Text :> Delete '[JSON] (DataWrapped ())
    -- Misc routes
    --   viewInit :: route :- "view" :> "init" :> BearerAuth :> Get '[JSON] (DataWrapped InitData)
    emailSubscribe :: route :- "email" :> ReqBody '[JSON] EmailSubscription :> Post '[JSON] (DataWrapped EmailSubscribeOk)
  }
  deriving (Generic)

terminalCoffee :: TerminalCoffeeRoutes (AsClientT ClientM)
terminalCoffee = genericClient

call :: ClientM a -> IO (Either ClientError a)
call endpoint = do
  let log request = do
        appendFile "whitepaper-log-request.txt" (groom request)
        case request.requestBody of
          RequestBodyLBS body -> appendFileLBS "whitepaper-log-request.txt" body
          RequestBodyBS body -> appendFileBS "whitepaper-log-request.txt" body
          _ -> appendFile "whitepaper-log-request.txt" "cant print body"
        pure request
  let baseUrl = BaseUrl Https "api.dev.terminal.shop" 443 ""
  mgr <-
    newManager
      tlsManagerSettings
        { managerModifyRequest = log
        }
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

callEndpoint ::
  (ToLatex a, Show a) =>
  -- | The API call to make
  ClientM a ->
  -- | Don't worry about it
  Q [Dec]
callEndpoint doCall = do
  liftIO (List.lookup "COFFEETIME" <$> getEnvironment)
    >>= maybe (pure []) \_ -> do
      liftIO (call doCall) >>= \case
        Left err -> do
          appendFile "whitepaper-log-response.txt" (groom err)
          appendFile "whitepaper-log-response.txt" "\n"
          renderResponse err
        Right res -> do
          appendFile "whitepaper-log-response.txt" (groom res)
          appendFile "whitepaper-log-response.txt" "\n"
          renderResponse res

renderResponse :: (ToLatex a) => a -> Q [Dec]
renderResponse response = do
  liftIO $ writeLatext response
  let name = mkName "yourProgram"
  liftIO $ callProcess "pdflatex" ["-interaction=nonstopmode", "Whitepaper.tex"]
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
  documentTitle :: String
  inThisPaper :: String
  toLatex :: a -> LaTeXT IO ()
  appendix :: a -> Maybe (LaTeXT IO ())
  appendix = const Nothing

instance ToLatex Product where
  documentTitle = "Analysis of Coffee Product Specifications and Market Positioning"
  inThisPaper = "Present a quantitative analysis of coffee product distribution patterns and evaluate market-specific pricing optimization strategies."

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

instance ToLatex [Product] where
  documentTitle = "Comprehensive Analysis of Global Coffee Product Distribution and Pricing Strategies"
  inThisPaper = "Conduct a comprehensive evaluation of global coffee distribution networks and develop a theoretical framework for cross-regional pricing optimization."

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
  documentTitle = "A Study in Nothingness: Void Operations in Coffee Systems"
  inThisPaper = "Examine the theoretical implications of null operations in distributed beverage procurement systems."
  toLatex () = "henlo"

instance ToLatex ClientError where
  documentTitle = "Error Patterns in Distributed Coffee Procurement: A Case Study"
  inThisPaper = "Investigate failure modes in distributed coffee procurement networks and propose resilience strategies for mission-critical beverage systems."

  toLatex err = do
    section "Critical Analysis: API Communication Breakdown"

    "This section examines a critical failure in the coffee procurement system's communication layer."

    par
    subsection "Error Classification"
    case err of
      FailureResponse _ response -> do
        textbf "Server Response Error Analysis"
        par
        "The server rejected our request with status code: "
        fromString (show $ responseStatusCode response)
        par
        "This indicates a fundamental misalignment between client expectations and server requirements."
      DecodeFailure msg _ -> do
        textbf "Data Structure Impedance Mismatch"
        par
        "Failed to decode server response: "
        fromString (show msg)
        par
        "This suggests a potential schema evolution or version mismatch between client and server."
      UnsupportedContentType mediaType _ -> do
        textbf "Protocol Negotiation Failure"
        par
        "Server responded with unsupported content type: "
        fromString (show mediaType)
        par
        "This indicates a fundamental protocol mismatch in content negotiation."
      InvalidContentTypeHeader _ -> do
        textbf "Protocol Specification Violation"
        par
        "The server provided an invalid content-type header."
        par
        "This suggests potential middleware interference or server misconfiguration."
      ConnectionError ex -> do
        textbf "Network Layer Communication Failure"
        par
        "Failed to establish connection: "
        fromString (show ex)
        par
        "This points to infrastructure or network stability issues."

    par
    subsection "Impact Assessment"
    "The observed failure has significant implications for the system's reliability:"
    itemize $ do
      item Nothing <> "Disruption of the coffee procurement pipeline"
      item Nothing <> "Potential loss of caffeine-dependent productivity"
      item Nothing <> "Risk of developers resorting to tea consumption"

    par
    subsection "Mitigation Strategies"
    "Recommended actions to prevent future failures:"
    itemize $ do
      item Nothing <> "Implement robust error handling mechanisms"
      item Nothing <> "Establish backup coffee procurement channels"
      item Nothing <> "Consider local coffee machine installation"
      item Nothing <> "Maintain emergency instant coffee supplies"
  appendix = \case
    FailureResponse _request response -> Just do
      LaTeX.appendix
      section "Server Response"
      "The server responded with status code: " <> fromString (show response.responseStatusCode)
      par
      fromLaTeX $
        TeXEnv
          "lstlisting"
          [OptArg "caption=Server Response, breaklines=true"]
          (raw ("\n" <> decodeUtf8 response.responseBody))
    _ -> Nothing

instance ToLatex EmailSubscribeOk where
  documentTitle = "A Personal Journey into Coffee Research Networks: Subscription Analysis"
  inThisPaper = "Document the integration process into professional coffee research networks and analyze the implications for academic discourse"

  toLatex SimplyOk = do
    section "Documentation of Research Network Integration"

    "This section examines our successful subscription to the Terminal Coffee research network, "
    "marking a significant step in our ongoing investigation of beverage procurement systems."

    par
    subsection "Subscription Confirmation Analysis"
    "The successful subscription represents several key achievements:"
    itemize $ do
      item Nothing <> "Integration into the coffee research community"
      item Nothing <> "Access to forthcoming studies and findings"
      item Nothing <> "Potential for collaborative research opportunities"

    par
    subsection "Expected Research Benefits"
    "This subscription will facilitate:"
    itemize $ do
      item Nothing <> "Early access to groundbreaking coffee-related research"
      item Nothing <> "Participation in peer review processes"
      item Nothing <> "Direct communication with leading coffee scientists"

    par
    subsection "Future Research Directions"
    "With this subscription established, we propose to:"
    itemize $ do
      item Nothing <> "Monitor emerging trends in coffee science"
      item Nothing <> "Contribute to ongoing discussions in the field"
      item Nothing <> "Prepare responses to forthcoming publications"

instance ToLatex [Card] where
  documentTitle = "Financial Instruments in Coffee Procurement: A Study of Payment Methods"
  inThisPaper = "Analyze the diversity and security implications of payment methods in automated coffee distribution systems."

  toLatex cards = do
    section "Analysis of Payment Methods in Coffee Distribution"

    "This section examines the registered payment instruments in the coffee procurement system."

    par
    subsection "Payment Method Statistics"
    "The system currently has " <> fromString (show (length cards)) <> " registered payment methods."

    par
    subsection "Security Considerations"
    "All payment methods are stored in accordance with PCI DSS requirements, "
    "ensuring the highest level of security in beverage-related transactions."

    par
    subsection "Payment Method Distribution"
    "The following analysis presents the distribution of payment methods:"

    -- Card brand distribution

    let brandCounts :: [(Text, Int)]
        brandCounts =
          List.sortOn (negate . snd) $
            mapMaybe (viaNonEmpty (\xs@(x :| _) -> (x.brand, length xs))) $
              List.groupBy (\a b -> a.brand == b.brand) $
                List.sortOn (.brand) cards

    "Brand Distribution Analysis:"
    center $
      tabular Nothing [VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine] $ do
        hline
        "Card Brand" & "Count" <> lnbk
        hline
        mconcat
          [ fromString (toString brand) & fromString (show count) <> lnbk
          | (brand, count) <- brandCounts
          ]
        hline

    par
    "Detailed Card Inventory:"
    if null cards
      then "<E_NOT_FOUND>"
      else itemize $ do
        mconcat
          [ item Nothing
            <> fromString (show card.brand)
            <> " ending in "
            <> fromString (show card.last4)
            <> " (expires "
            <> fromString (show card.expiration.month)
            <> "/"
            <> fromString (show card.expiration.year)
            <> ")"
            <> lnbk
            <> "identified by ID: "
            <> fromString (show card.id)
          | card <- cards
          ]

    par
    "Security Considerations:"
    itemize $ do
      item Nothing <> "Total registered cards: " <> fromString (show (length cards))
      item Nothing <> "All card numbers are truncated for security (last 4 digits only)"
      item Nothing <> "Cards are tokenized and stored according to PCI DSS requirements"

    par
    subsection "Implications for Coffee Acquisition"
    "The presence of multiple payment methods suggests:"
    itemize $ do
      item Nothing <> "Robust failover capabilities in coffee procurement"
      item Nothing <> "Distributed risk across payment instruments"
      item Nothing <> "Enhanced reliability in caffeine acquisition workflows"

instance ToLatex CardCollectLink where
  documentTitle = "Secure Payment Method Collection in Distributed Coffee Systems: A Protocol Analysis"
  inThisPaper = "Examine the security implications and user experience considerations of delegated payment information collection in distributed beverage procurement systems."

  toLatex (CardCollectLink{url}) = do
    section "Analysis of Secure Payment Collection Protocol"

    "This section examines the implementation of a secure payment method collection system, "
    "specifically focusing on the delegation of sensitive financial data handling to a specialized service."

    par
    subsection "Protocol Overview"
    "The system has generated a temporary, single-use URL for secure payment method collection:"
    quote $ fromString (toString url)

    par
    subsection "Security Considerations"
    "The temporary URL mechanism provides several security benefits:"
    itemize $ do
      item Nothing <> "Time-limited exposure window"
      item Nothing <> "Single-use nature prevents replay attacks"
      item Nothing <> "Delegation of PCI compliance to specialized service"
      item Nothing <> "Isolation of sensitive data from main application"

    par
    subsection "Implementation Details"
    "The card collection process follows these steps:"
    enumerate $ do
      item Nothing <> "Generate temporary URL with limited lifetime"
      item Nothing <> "Redirect user to secure collection interface"
      item Nothing <> "Collect and validate payment information"
      item Nothing <> "Return tokenized card reference"
      item Nothing <> "Invalidate temporary URL"

    par
    subsection "Risk Analysis"
    "Key security considerations in the implementation:"
    itemize $ do
      item Nothing <> "URL must be transmitted securely to intended user"
      item Nothing <> "Short expiration window reduces exposure risk"
      item Nothing <> "System never directly handles raw card data"
      item Nothing <> "Tokenization provides additional security layer"

instance ToLatex [AddressWithId] where
  documentTitle = "Geographic Distribution Analysis of Coffee Delivery Endpoints"
  inThisPaper = "Analyze the spatial distribution of coffee delivery locations and evaluate logistical optimization opportunities in beverage distribution networks."

  toLatex addresses = do
    section "Analysis of Coffee Distribution Endpoints"

    "This section examines " <> fromString (show (length addresses)) <> " registered delivery locations "
    "in the coffee distribution network."

    par
    subsection "Geographic Distribution"
    "Distribution of delivery endpoints by country:"
    let countryCounts =
          List.sortOn (negate . snd) $
            mapMaybe (viaNonEmpty (\xs@(x :| _) -> (x.country, length xs))) $
              List.groupBy (\a b -> a.country == b.country) $
                List.sortOn (.country) addresses
    center $
      tabular Nothing [VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine] $ do
        hline
        "Country" & "Address Count" <> lnbk
        hline
        mconcat
          [ fromString (toString country) & fromString (show count) <> lnbk
          | (country, count) <- countryCounts
          ]
        hline

    par
    subsection "Detailed Address Analysis"
    if null addresses
      then "No delivery locations currently registered."
      else itemize $ do
        mconcat
          [ item Nothing
            <> textbf (fromString $ toString addr.name)
            <> newline
            <> fromString (toString addr.street1)
            <> (if null (toString addr.street2) then "" else newline <> fromString (toString addr.street2))
            <> newline
            <> fromString (toString addr.city)
            <> ", "
            <> fromString (toString addr.province)
            <> " "
            <> fromString (toString addr.zip)
            <> newline
            <> fromString (toString addr.country)
            <> newline
            <> "Phone: "
            <> fromString (toString addr.phone)
            <> newline
            <> "ID: "
            <> fromString (toString addr.id)
          | addr <- addresses
          ]

    par
    subsection "Logistical Implications"
    "The geographic distribution of delivery endpoints suggests:"
    itemize $ do
      item Nothing <> "Multiple delivery zones requiring coordinated logistics"
      item Nothing <> "Varied regional coffee consumption patterns"
      item Nothing <> "Opportunities for delivery route optimization"
      item Nothing <> "Need for region-specific delivery time considerations"

instance ToLatex AddressID where
  documentTitle = "On the Generation and Properties of Unique Address Identifiers in Coffee Distribution Systems"
  inThisPaper = "Examine the characteristics and implications of newly generated address identifiers in distributed coffee delivery networks."

  toLatex (AddressID addressId) = do
    section "Analysis of Generated Address Identifier"

    "This paper presents a detailed examination of a newly generated address identifier "
    "within the context of coffee distribution systems."

    par
    subsection "Identifier Properties"
    "The system has generated the following unique identifier:"
    quote $ fromString (toString addressId)

    par
    subsection "Technical Implications"
    "The generation of this identifier represents several key system properties:"
    itemize $ do
      item Nothing <> "Uniqueness guarantees across the distribution network"
      item Nothing <> "Temporal correlation with address registration events"
      item Nothing <> "Integration with existing delivery routing systems"
      item Nothing <> "Compatibility with cross-regional coffee logistics"

    par
    subsection "System Integration Analysis"
    "The newly generated identifier facilitates:"
    itemize $ do
      item Nothing <> "Unambiguous address reference in future orders"
      item Nothing <> "Efficient lookup in delivery optimization algorithms"
      item Nothing <> "Integration with existing coffee routing systems"
      item Nothing <> "Audit trail capabilities for delivery analytics"

    par
    subsection "Practical Applications"
    "This identifier will now serve as a permanent reference point for:"
    itemize $ do
      item Nothing <> "Future coffee delivery operations"
      item Nothing <> "Address validation processes"
      item Nothing <> "Geographic distribution analytics"
      item Nothing <> "Delivery optimization calculations"

instance ToLatex OrderCreated where
  documentTitle = "Accidental Discoveries in Computer Science: When Haskell Compilation Meets Coffee Procurement"
  inThisPaper = "Explore the unexpected correlation between Haskell compilation attempts and scientific paper generation, with implications for automated research production."

  toLatex (OrderCreated orderId) = do
    section "Breakthrough: From Compilation Error to Scientific Discovery"

    "In an unprecedented turn of events, what started as a routine attempt to compile a Haskell application "
    "has led to an unexpected breakthrough in the field of computer science and beverage procurement systems."

    par
    textbf "Key Achievement: "
    "Successfully generated Order ID: " <> fromString (show orderId)

    par
    subsection "Methodology"
    itemize $ do
      item Nothing <> "Attempted to compile a Haskell application"
      item Nothing <> "Encountered expected compilation errors"
      item Nothing <> "Accidentally produced a scientific paper"
      item Nothing <> "Successfully ordered coffee"

    par
    subsection "Impact Analysis"
    quote $ do
      textit "This groundbreaking work demonstrates that while Haskell applications may not always compile as intended, "
      "they consistently produce valuable scientific literature and, more importantly, facilitate the acquisition of caffeine."

    par
    subsection "Future Work"
    "Further research is needed to explore the correlation between compilation failures and scientific paper generation. "
    "The authors suggest investigating whether different error messages could produce papers in various scientific fields."

    par
    center $ textbf "The coffee has been ordered. The paper has been written. Science has been advanced."

writeLatext :: forall a. (ToLatex a) => a -> IO ()
writeLatext a = do
  authorName <- liftIO invasivelyGatherPII
  doc <- execLaTeXT do
    documentclass [] article
    usepackage [] "listings"
    author $ fromString authorName
    title $ fromString $ documentTitle @a
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

      toLatex a

      section "Conclusion"
      "This paper presented an implementation and analysis of a client-side coffee ordering system utilizing RESTful API endpoints. The system demonstrated the practical application of HTTP requests to interact with a remote coffee service API, enabling users to place beverage orders programmatically. The implementation showcased standard REST practices, including GET requests to retrieve available menu items and POST requests to submit orders. The paper discussed the handling of API responses, error cases, and the structured JSON payload format used for order specifications, including customization options such as size, temperature, and additional ingredients. Authentication mechanisms and order tracking functionality were also examined. This work provided insights into modern API consumption patterns and served as a practical example of integrating third-party services into client applications for automated beverage ordering."

      section "Future Work"
      "Several promising directions for future research have emerged from this work:"
      itemize $ do
        item Nothing <> "Investigation into the correlation between coffee consumption and code quality, with particular focus on the optimal caffeine levels for maintaining type safety in Haskell programs"
        item Nothing <> "Exploration of the metaphysical properties of mysterious orbs and their potential applications in software architecture design"
        item Nothing <> "Development of a theoretical framework for understanding why we keep writing software despite knowing better"
        item Nothing <> "Analysis of the relationship between late-night coding sessions, coffee intake, and the probability of accidentally creating skynet"
        item Nothing <> "Quantum entanglement studies between programmers and their rubber duck debugging companions"

      "The authors acknowledge that some of these research directions may be heavily influenced by excessive coffee consumption and prolonged exposure to terminal screens."

      whenJust (appendix a) id

  renderFile "Whitepaper.tex" doc
