{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Identity (Identity(..))
import Control.Monad.Writer (Writer)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Text.Ginger ((~>), GVal, IncludeResolver, Run, SourcePos, Template, ToGVal(..), dict, easyRender, parseGinger)

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

template :: Template SourcePos
template = either (error . show) id . runIdentity $
  parseGinger nullResolver Nothing "{{ word }}"

-- | Render a template using a simple HashMap context
renderHashMap :: IO ()
renderHashMap = do
    let ctx :: HashMap Text Text
        ctx = HashMap.fromList
                [ ("word", "bird")
                ]
    Text.putStrLn $ easyRender ctx template

-- | Render a template using a HashMap converted to a GVal
renderGVal :: IO ()
renderGVal = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = toGVal $ HashMap.fromList
                [ ("word" :: Text, "bird" :: Text)
                ]
    Text.putStrLn $ easyRender ctx template

-- | Render a template using a heterogeneous dictionary context
renderDict :: IO ()
renderDict = do
    let ctx :: GVal (Run SourcePos (Writer Text) Text)
        ctx = dict
                [ (("word" :: Text) ~> ("bird" :: Text))
                ]
    Text.putStrLn $ easyRender ctx template

main :: IO ()
main = renderHashMap >> renderGVal >> renderDict
