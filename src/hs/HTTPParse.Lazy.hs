{-# LANGUAGE OverloadedStrings #-}

module HTTPParse.Lazy (
      Parser
    , runParser
    , HTTPMethod (..)
    , HTTPStart (..)
    , httpReqStart
    , httpHeaderField
    , httpHeaderFields
    , httpHeaderFieldsWithCRLF
    , httpRequestInfo
    , httpEmptyLine
    , httpHeaders ) where

import GHC.Int
import Data.Char
import Data.List
import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import Data.Either
import Control.Applicative
import Control.Monad

import Debug.Trace

-- type Bytes = [Word8]

newtype Parser a = Parser ( ByteString -> [(a, ByteString)] )

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser (\ws -> [(a, ws)])
    (<*>) = ap

runParser (Parser f) = f

instance Monad Parser where
    -- f :: a -> Parser b
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    m >>= f = Parser (\ws -> let l = runParser m ws
                                 f' (a, ws') = runParser (f a) ws'
                             in l >>= f')

-- Failed/empty parse
empt :: Parser a
empt = Parser (\_ -> [])

-- Choice
p <+> p' = Parser (\ws -> runParser p ws ++ runParser p' ws)

-- Biased choice: pick first one if success
p </> p' = Parser (\ws -> let res = runParser p ws
                          in case res of
                                  (r:rs) -> res
                                  _      -> runParser p' ws)

parseNothing = return ()
parseEither p1 p2 = (Left <$> p1) </> (Right <$> p2)

repetition p = do
    res <- parseEither p parseNothing
    case res of
        Left a -> do as <- repetition p
                     return (a:as)
        _      -> return []

-- Optional parser
optl :: Parser a -> Parser (Maybe a)
optl p = (liftM Just p) </> (return Nothing)


-- Filtering
filtr :: (a -> Bool) -> Parser a -> Parser a
filtr pred parse = do
    res <- parse
    if pred res then return res
                else empt


-- Get byte
byte :: Parser Word8
byte = Parser _byte

char :: Parser Char
char = Parser _char


_byte :: ByteString -> [(Word8, ByteString)]
_byte bs | BS.null bs = []
         | otherwise  = [(BS.head bs, BS.tail bs)]

_char :: ByteString -> [(Char, ByteString)]
_char bs | CS.null bs = []
         | otherwise  = [(CS.head bs, CS.tail bs)]


-- Get predetermined byte
litByte :: Word8 -> Parser Word8
litByte c = filtr (c ==) byte

lit c = filtr (== c) char

-- Get n bytes
bytes :: Int64 -> Parser ByteString
bytes n = Parser (bytesInternal n)

bytesInternal n bs = let first = BS.take n bs
                         rest  = BS.drop n bs in
                         if BS.length first == n then
                             [(first, rest)]
                         else
                             []

takeWhileChar :: (Char -> Bool) -> Parser ByteString
takeWhileChar pred = Parser $ (: []) . CS.span pred

-- Get predetermined byte sequence
byteSeq :: ByteString -> Parser ByteString
byteSeq bs = do
    bs' <- bytes (BS.length bs)
    if bs' == bs then return bs
                 else empt

data HTTPMethod = Get
                | Post
                | Head
                | Delete
    deriving (Show, Eq)

data HTTPStart = HTTPReqStart HTTPMethod ByteString (Int, Int)
    deriving (Show, Eq)

digit = digitToInt <$> filtr isDigit char

httpGet = byteSeq "GET" >> return Get
httpPost = byteSeq "POST" >> return Post
httpHead = byteSeq "HEAD" >> return Head
httpDelete = byteSeq "DELETE" >> return Delete

httpMethod = httpGet
         </> httpPost
         </> httpHead
         </> httpDelete

-- Character classes somewhat approximate here, but for correct definition, see
-- RFCs 7230 and 3986

pathChar c = 
        isAlpha c
     || isDigit c
     || c `elem` ("-._~%" :: String) -- unreserved, % from pct-encoded
     || c `elem` ("!$&'()*+,;=" :: String) -- sub-delims
     || c `elem` ("/?:@" :: String) -- /? from query (? also origin-form), :@ from pchar

httpOriginPath = takeWhileChar $ pathChar

httpReqStart :: Parser HTTPStart
httpReqStart = do
    method <- httpMethod
    lit ' '
    path <- httpOriginPath
    lit ' '
    byteSeq "HTTP/"
    major <- digit
    lit '.'
    minor <- digit
    byteSeq "\r\n"
    return (HTTPReqStart method path (major, minor))

fieldName = token

token = filtr (not . BS.null) $ takeWhileChar tokenChar
tokenChar c = isDigit c || isAlpha c || c `elem` ("!#$%&'*+-.^_`|~" :: String)

isVChar = (\d -> d >= 0x21 && d <= 0x7e) . ord
isObsText = (>= 0x80) . ord -- We are working with 8 bit chars here
isWsp c = c == ' ' || c == '\t'

fieldValue = Parser $ \bs -> case countFieldValue 0 bs of
                                Just (i, bs'') -> [(BS.take i bs, bs'')]
                                _              -> []

countFieldValue cur bs =
    case countWs bs of 
        Just (wslen, bs') ->
            case countFieldChar bs' of
                Just (vcharlen, bs'') ->
                    if vcharlen > 0 then
                        countFieldValue (cur + wslen + vcharlen) bs''
                    else
                        Just (cur, bs'')
                _              -> Nothing
        _ -> Nothing

countWs bs = case CS.findIndex (not . isWsp) bs of
    Just i -> Just (i, BS.drop i bs)
    _      -> Nothing
countFieldChar bs = case CS.findIndex (not . (\c -> isVChar c || isObsText c)) bs of
    Just i -> Just (i, BS.drop i bs)
    _      -> Nothing

httpEmptyLine = do
    byteSeq "\r\n"
    return ()

ows = takeWhileChar isWsp

-- does not accept obs-fold
httpHeaderField :: Parser (ByteString, ByteString)
httpHeaderField = do
    key <- fieldName
    lit ':'
    ows
    value <- fieldValue -- fieldValue strips leading and trailing opt. whitespace
    byteSeq "\r\n"
    return (key, value)

httpHeaderFieldsWithCRLF = do
    res <- parseEither httpHeaderField httpEmptyLine
    case res of
        Left field -> do fields <- httpHeaderFields
                         return $ field:fields
        _          -> return []

httpHeaderFields = repetition httpHeaderField

httpHeaders = do
    hdrs <- httpHeaderFields
    end <- optl (byteSeq "\r\n")
    case end of
        Just _ -> return (hdrs, True)
        _      -> return (hdrs, False)

httpRequestInfo = do
    start <- httpReqStart
    fields <- httpHeaderFields
    return (start, fields)
