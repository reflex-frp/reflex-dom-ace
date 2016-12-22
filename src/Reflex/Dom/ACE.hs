{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.ACE where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Map (Map)
import           Data.Text (Text)
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------


newtype AceRef = AceRef { unAceRef :: JSVal }

data ACE t = ACE
    { aceRef   :: Dynamic t (Maybe AceRef)
    , aceValue :: Dynamic t Text
    }

data AceConfig = AceConfig
    { aceConfigBasePath :: Text
    , aceConfigMode     :: Text
    , aceElemId         :: Text
    , aceElemAttrs      :: Map Text Text
    }

------------------------------------------------------------------------------
startACE
    :: AceConfig
    -> IO AceRef
#ifdef ghcjs_HOST_OS
startACE ac =
    js_startACE (toJSString $ aceElemId ac)
                (toJSString $ aceConfigBasePath ac)
                (toJSString $ aceConfigMode ac)

foreign import javascript unsafe
  "(function(){ ace['config']['set']('basePath', $2); var a = ace['edit']($1); a['session']['setMode']($3); return a; })()"
  js_startACE :: JSString -> JSString -> JSString -> IO AceRef

#else
startACE = error "startACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
moveCursorToPosition :: (Int, Int) -> AceRef -> IO ()
#ifdef ghcjs_HOST_OS
moveCursorToPosition (r,c) a = js_moveCursorToPosition a r c

foreign import javascript unsafe
  "(function(){ $1['gotoLine']($2, $3, true); })()"
  js_moveCursorToPosition :: AceRef -> Int -> Int -> IO ()
#else
moveursorToPosition = error "moveCursorToPosition: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setModeACE :: MonadIO m => Text -> AceRef -> m ()
#ifdef ghcjs_HOST_OS
setModeACE mode = liftIO . js_aceSetMode (toJSString mode)

foreign import javascript unsafe
  "(function(){ return $1['session']['setMode']($2); })()"
  js_aceSetMode :: JSString -> AceRef -> IO ()
#else
setModeACE = error "setModeACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setConfigACE :: MonadIO m => Text -> Text -> AceRef -> m ()
#ifdef ghcjs_HOST_OS
setConfigACE key val = liftIO . js_aceSetConfig (toJSString key) (toJSString val)

foreign import javascript unsafe
  "(function(){ return $3['config']['set']($1, $2); })()"
  js_aceSetConfig :: JSString -> JSString -> AceRef -> IO ()
#else
setConfigACE = error "setConfigACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
getValueACE :: MonadIO m => AceRef -> m Text
#ifdef ghcjs_HOST_OS
getValueACE a = liftIO $ fromJSString <$> js_aceGetValue a

foreign import javascript unsafe
  "(function(){ return $1['getValue'](); })()"
  js_aceGetValue :: AceRef -> IO JSString
#else
getValueACE = error "getValueACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setValueACE :: MonadIO m => Text -> AceRef -> m ()
#ifdef ghcjs_HOST_OS
setValueACE v a = liftIO $ js_aceSetValue a (toJSString v)

foreign import javascript unsafe
  "(function(){ $1['setValue']($2, -1); })()"
  js_aceSetValue :: AceRef -> JSString -> IO ()
#else
setValueACE = error "setValueACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setupValueListener :: MonadWidget t m => AceRef -> m (Event t Text)
#ifdef ghcjs_HOST_OS
setupValueListener ace = do
    pb <- getPostBuild
    let act cb = liftIO $ do
          jscb <- asyncCallback1 $ \_ -> liftIO $ do
              v <- getValueACE ace
              cb v
          js_setupValueListener ace jscb
    performEventAsync (act <$ pb)

foreign import javascript unsafe
  "(function(){ $1['on'](\"change\", $2); })()"
  js_setupValueListener :: AceRef -> Callback (JSVal -> IO ()) -> IO ()
#else
setupValueListener = error "setupValueListener: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
aceWidget :: MonadWidget t m => AceConfig -> Text -> m (ACE t)
aceWidget ac initContents = do
    elAttr "div" ("id" =: aceElemId ac <> aceElemAttrs ac) $
      text initContents

    pb <- getPostBuild
    aceUpdates <- performEvent (liftIO (startACE ac) <$ pb)
    res <- widgetHold (return never) $ setupValueListener <$> aceUpdates
    aceDyn <- holdDyn Nothing $ Just <$> aceUpdates
    updatesDyn <- holdDyn initContents $ switchPromptlyDyn res

    return $ ACE aceDyn updatesDyn


------------------------------------------------------------------------------
-- | Convenient helper function for running functions that need an AceRef.
withAceRef
    :: PerformEvent t m
    => ACE t
    -> Event t (AceRef -> Performable m ())
    -> m (Event t ())
withAceRef ace evt = withAceRef' ace (f <$> evt)
  where
    f _ Nothing = return ()
    f g (Just a) = g a


------------------------------------------------------------------------------
-- | More powerful function for running functions that need an AceRef.
withAceRef'
    :: PerformEvent t m
    => ACE t
    -> Event t (Maybe AceRef -> Performable m a)
    -> m (Event t a)
withAceRef' ace val =
    performEvent $ attachPromptlyDynWith (flip ($)) (aceRef ace) val


