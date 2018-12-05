{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Text    (Text, unlines, append)
import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmEncoderSource, toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:<|>), (:>), Capture, Get, JSON, Post, ReqBody)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               UrlPrefix (Variable), defElmImports, defElmOptions,
                               generateElmForAPIWith)

data Book = Book
  { name :: String
  } deriving (Show, Eq, Generic)

instance ElmType Book

type BooksApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
           :<|> "books" :> Get '[JSON] [Book]
           :<|> "books" :> Capture "bookId" Int :> Get '[JSON] Book

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Variable "Config.api" }

myElmImports :: Text
myElmImports = Data.Text.append defElmImports $
                 Data.Text.unlines [ "import Config" ]

spec :: Spec
spec = Spec ["Generated", "BooksApi"]
            (myElmImports
             : toElmTypeSource    (Proxy :: Proxy Book)
             : toElmDecoderSource (Proxy :: Proxy Book)
             : toElmEncoderSource (Proxy :: Proxy Book)
             : generateElmForAPIWith myElmOpts (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "elm"
