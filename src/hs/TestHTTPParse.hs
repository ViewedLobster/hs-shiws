{-# LANGUAGE OverloadedStrings #-}
import Debug.Trace

import HTTPParse.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy.Internal (ByteString (..) )


main = do
    let httpRequest = L.fromChunks ["GET /hello-world HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n", (trace "ate the new line" "thisisthenextline"), "\r\nyetanothernewline"]
    putStrLn $ (show . fst . head) (runParser httpReqStart httpRequest)
    --putStrLn $ show (C.take 50 httpRequest)
