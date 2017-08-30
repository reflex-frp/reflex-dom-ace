{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Control.Monad                    (forM_, void, msum, join)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Maybe                       (maybeToList)
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex.Dom                       hiding (run)
import qualified Reflex.Dom.Core                  as RDC
import           System.Directory                 (getCurrentDirectory)
import           Text.Read                        (readMaybe)

import           Reflex.Dom.ACE


fileText :: Text
fileText = T.unlines
  [ "Here is the first line."
  , "Here are some quotes from I <3 Huckabees:"
  , "  Honestly, I have no idea what you guys are talking about. I thought we were talking about petroleum."
  , "  If this world is temporary, identity is an illusion, then everything is meaningless and it doesn't matter if you use petroleum, and that's got me very confused."
  , "  We're not in Infinity, we're in the suburbs."
  , "  Everything is the same, even if it's different."
  , "  I can't believe you guys actually exist."
  , "  How am I not myself?"
  , ""
  , "  Mrs. Hooten: So Tommy, what do you do?"
  , "  Tommy Corn : I'm a firefighter."
  , "  Mrs. Hooten: God bless you! You're a hero."
  , "  Tommy Corn : I'm no hero. We'd all be heroes if we stopped using petroleum!"
  ]


defaultUrl :: Text
defaultUrl = "http://localhost:3000"


exampleAnnotations :: [Annotation]
exampleAnnotations =
  [ Annotation 0 0  "first line"           AnnotationWarning
  , Annotation 5 30 "identity is monoidal" AnnotationError
  , Annotation 7 0  "how am i not myself?" AnnotationWarning
  ]


readAnnotation :: Text -> Maybe Annotation
readAnnotation = readMaybe . T.unpack


manyAnnotations :: Text -> [Annotation]
manyAnnotations = join . maybeToList . readMaybe . T.unpack


singleAnnotation :: Text -> [Annotation]
singleAnnotation = maybeToList . readAnnotation


anyAnnotations :: Text -> [Annotation]
anyAnnotations txt = msum [manyAnnotations txt, singleAnnotation txt]


app :: forall t m. MonadWidget t m => Text -> m ()
app url = do
  elAttr "style" ("type" =: "text/css" <> "media" =: "screen") $
    text $ T.unlines
      [ "#ace-editor { width:100%; height:100%; }"
      , "#editor { position:relative; height:400px; left:-10px; padding:10px; }"
      , "body { width:100%; height:100%; }"
      , "input { width:600px; }"
      ]
  let src = T.append url "/ace.js"
  (script, _ ) <- elAttr' "script" ("src" =: src) blank
  let evScriptLoaded :: Event t ()
      evScriptLoaded = () <$ domEvent Load script
      loading = el "pre" $ text "loading..."
  void $ widgetHold loading $ ffor evScriptLoaded $ const $ do
    ace <- elAttr "div" ("id" =: "editor") $ do
      let cfg = def{ _aceConfigBasePath        = Just "/"
                   , _aceConfigElemAttrs       = "id" =: "ace-editor"
                   , _aceConfigWordWrap        = True
                   , _aceConfigShowPrintMargin = True
                   }
      aceWidget cfg (AceDynConfig Nothing) never fileText
    (btn, _) <- el' "button" $ text "Set annotations:"
    el "br" blank
    ti <- textInput def
    el "pre" $ text $ T.unlines $ map (T.pack . show) exampleAnnotations
    let evText = tagPromptlyDyn (value ti) $ domEvent Click btn
    dAnnotations <- foldDyn (\txt _ -> anyAnnotations txt) [] evText
    let evAnnotations = updated dAnnotations
    void $ withAceInstance ace $ setAnnotations <$> evAnnotations


runApp :: Text -> IO ()
runApp = run 8888 . RDC.mainWidget' . app


runDef :: IO ()
runDef = runApp defaultUrl


main :: IO ()
main = putStrLn "load this up in a repl and call 'runApp urlToLibDir'"
