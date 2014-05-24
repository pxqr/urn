-- |
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   URN parsing and rendering according to RFC2141.
--
--   For more info see: <http://tools.ietf.org/html/rfc2141>
--
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Data.URN
       ( NID
       , NSS
       , URN (..)
       , renderURN
       , parseURN
       ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List as L
import Data.String
#if defined(__GLASGOW_HASKELL__)
import Data.Data
import Data.Typeable ()
#endif
import Text.Parsec as P
import Text.Parsec.String
import Text.Read
import Numeric


-- | Namespace Identifier determines the syntactic interpretation of
-- namespace-specific string.
type NID = String

-- | Namespace Specific String.
type NSS = String

-- | URN is persistent location-independent identifier for
--   resources. For example:
--
--   > urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6
--
data URN = URN
  { -- | a normalized namespace identifier;
    --
    -- > uuid
    --
    urnNamespace :: NID
    -- | the corresponding normalized and unescaped namespace-specific string.
    --
    -- > f81d4fae-7dec-11d0-a765-00a0c91e6bf6
    --
  , urnString    :: NSS
  } deriving
      ( Eq, Ord
#if defined(__GLASGOW_HASKELL__)
      , Typeable, Data
#endif
      )

-- TODO 2.4 Excluded characters
-- http://tools.ietf.org/html/rfc2141

isURNChar, isTrans, isOther, isReserved :: Char -> Bool
isURNChar  x = isTrans x || isHexDigit x
isTrans    x = isAlphaNum x || isOther x || isReserved x
isOther    x = L.elem x "()+,-.:=@;$_!*'"
isReserved x = L.elem x "%/?#"

escape :: String -> String
escape = concatMap f
  where
    f x
      | not (isURNChar x) || x == '%' = '%' : showHex (fromEnum x) ""
      |           otherwise           = [x]

unescape :: String -> Maybe String
unescape ('%' : a : b : xs) = do
  n   <- readMaybe ['0', 'x', a, b]
  xs' <- unescape xs
  return $ toEnum n : xs'
unescape ('%' : _)          = Nothing
unescape (x : xs)           = do
  xs' <- unescape xs
  return (x : xs')
unescape []                 = return []

-- | Render URN to its text representation. Resulting string will have
-- canonicalized 'NID' and 'NSS' part.
renderURN :: URN -> String
renderURN urn = "urn:" ++ nid ++ ":" ++ nss
  where
    nid = L.map toLower (urnNamespace urn)
    nss = escape (urnString urn)

-- | Parse URN from its text representation. Resulting 'URN' will have
-- canonicalized 'NID' and 'NSS' part.
parseURN :: String -> Maybe URN
parseURN = either (const Nothing) Just . parse purn ""
  where
    pscm :: Parser ()
    pscm = do
      _ <- oneOf "uU"
      _ <- oneOf "rR"
      _ <- oneOf "nN"
      _ <- char  ':'
      return ()

    pnid :: Parser NID
    pnid = do
      c  <- satisfy isAlphaNum
      cs <- some $ satisfy (\ x -> isAlphaNum x || x == '-')
      let nid = L.map toLower (c : cs)
      guard (nid /= "urn")
      return nid

    pnss :: Parser NSS
    pnss = do
      str <- P.many (satisfy isURNChar)
      case unescape str of
        Nothing  -> fail "parseURN: bad NSS escaping"
        Just nss -> return nss

    purn :: Parser URN
    purn = pscm >> URN <$> pnid <* char ':' <*> pnss

-- | Similar to 'parseURN'.
instance IsString URN where
  fromString t = case parseURN t of
    Nothing  -> error $ "fromString: unable to parse URN: " ++ show t
    Just urn -> urn

-- | Similar to 'renderURN'.
instance Show URN where
  showsPrec _ = showString . renderURN

-- | Similar to 'parseURN'.
instance Read URN where
  readsPrec _ xs =
      case parseURN urnStr of
        Nothing  -> []
        Just urn -> [(urn, rest)]
    where
      (urnStr, rest) = span (not . (== ' ')) xs