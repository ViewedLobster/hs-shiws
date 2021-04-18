{-# LANGUAGE OverloadedStrings #-}
import HTTPParse.Lazy
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

main = do
    let httpRequest = "GET /hello-world HTTP/1.1\r\n" `BS.append`
            "Transfer-Encoding: chunked      \r\nthisisthenextline"
    putStrLn $ show (runParser httpRequestInfo (L.fromStrict httpRequest))
