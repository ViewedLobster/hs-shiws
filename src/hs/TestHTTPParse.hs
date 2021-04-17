{-# LANGUAGE OverloadedStrings #-}
import HTTPParse
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

main = do
    let httpRequest = "GET /hello-world HTTP/1.1\r\n" `BS.append`
            "Transfer-Encoding: chunked\r\nthisisthenextline"
    putStrLn $ show (runParser httpRequestInfo httpRequest)
