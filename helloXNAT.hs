-- Copyright (c) 2014 Washington University School of Medicine
-- Author: Kevin A. Archie <karchie@wustl.edu>

{-# LANGUAGE OverloadedStrings #-}

{-
  Running:  XNAT_URL={url} JSESSIONID={jsid} runhaskell HelloXNAT
  prints the returned JSON for a /data/projects request
-}

import System.Environment (getEnv)
import System.IO (stdout)
import Network
import Network.HTTP.Conduit

import Data.Conduit.Binary (sinkHandle)

import qualified Control.Exception as E

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit as C

import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Binary as CB

import Network.HTTP.Types.Status


addJSESSIONID :: Request -> B.ByteString -> Request
addJSESSIONID req jsid =
  req { requestHeaders = jsidHeader : requestHeaders req }
  where
    jsidHeader = (CI.mk "Cookie", cookieset)
    cookieset = B.append "JSESSIONID=" jsid

addToPath :: Request -> B.ByteString -> Request
addToPath req relpath =
  req { path = B.append "/" (B.intercalate "/" components) }
  where
    components = filter (/= B.empty) $ concatMap (C8.split '/') [path req, relpath]


main = withSocketsDo $ do
  url <- getEnv "XNAT_URL"
  jsid <- getEnv "JSESSIONID"
  req' <- parseUrl url
  let req = addJSESSIONID req' (C8.pack jsid)
  let projreq = addToPath req "/data/projects"
  eitherResp <- E.try (withManager $ http projreq)
  case eitherResp of
    Left (StatusCodeException s _ _) -> putStrLn (show s)
    Right resp -> C.runResourceT $ do 
      responseBody resp C.$$+- sinkHandle stdout
