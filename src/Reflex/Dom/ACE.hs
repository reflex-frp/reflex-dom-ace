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
import           Data.Default
import           Data.Monoid
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------


data AceTheme
  = AceTheme_Chrome
  | AceTheme_Clouds
  | AceTheme_CrimsonEditor
  | AceTheme_Dawn
  | AceTheme_Dreamweaver
  | AceTheme_Eclipse
  | AceTheme_Github
  | AceTheme_Iplastic
  | AceTheme_SolarizedLight
  | AceTheme_Textmate
  | AceTheme_Tomorrow
  | AceTheme_Xcode
  | AceTheme_Kuroir
  | AceTheme_Katzenmilch
  | AceTheme_Sqlserver
  | AceTheme_Ambiance
  | AceTheme_Chaos
  | AceTheme_CloudsMidnight
  | AceTheme_Cobalt
  | AceTheme_Gruvbox
  | AceTheme_IdleFingers
  | AceTheme_KrTheme
  | AceTheme_Merbivore
  | AceTheme_MerbivoreSoft
  | AceTheme_MonoIndustrial
  | AceTheme_Monokai
  | AceTheme_PastelOnDark
  | AceTheme_SolarizedArk
  | AceTheme_Terminal
  | AceTheme_TomorrowNight
  | AceTheme_TomorrowNightBlue
  | AceTheme_TomorrowNightBright
  | AceTheme_TomorrowNightEighties
  | AceTheme_Twilight
  | AceTheme_VibrantInk
  deriving (Eq,Ord,Enum,Bounded)

instance Show AceTheme where
    show AceTheme_Ambiance = "ambiance"
    show AceTheme_Chaos = "chaos"
    show AceTheme_Chrome = "chrome"
    show AceTheme_Clouds = "clouds"
    show AceTheme_CloudsMidnight = "clouds_midnight"
    show AceTheme_Cobalt = "cobalt"
    show AceTheme_CrimsonEditor = "crimson_editor"
    show AceTheme_Dawn = "dawn"
    show AceTheme_Dreamweaver = "dreamweaver"
    show AceTheme_Eclipse = "eclipse"
    show AceTheme_Github = "github"
    show AceTheme_Gruvbox = "gruvbox"
    show AceTheme_IdleFingers = "idle_fingers"
    show AceTheme_Iplastic = "iplastic"
    show AceTheme_Katzenmilch = "katzenmilch"
    show AceTheme_KrTheme = "kr_theme"
    show AceTheme_Kuroir = "kuroir"
    show AceTheme_Merbivore = "merbivore"
    show AceTheme_MerbivoreSoft = "merbivore_soft"
    show AceTheme_MonoIndustrial = "mono_industrial"
    show AceTheme_Monokai = "monokai"
    show AceTheme_PastelOnDark = "pastel_on_dark"
    show AceTheme_SolarizedArk = "solarized_ark"
    show AceTheme_SolarizedLight = "solarized_light"
    show AceTheme_Sqlserver = "sqlserver"
    show AceTheme_Terminal = "terminal"
    show AceTheme_Textmate = "textmate"
    show AceTheme_Tomorrow = "tomorrow"
    show AceTheme_TomorrowNight = "tomorrow_night"
    show AceTheme_TomorrowNightBlue = "tomorrow_night_blue"
    show AceTheme_TomorrowNightBright = "tomorrow_night_bright"
    show AceTheme_TomorrowNightEighties = "tomorrow_night_eighties"
    show AceTheme_Twilight = "twilight"
    show AceTheme_VibrantInk = "vibrant_ink"
    show AceTheme_Xcode = "xcode"

data AceConfig = AceConfig
    { _aceConfigElemId    :: Text
    , _aceConfigElemAttrs :: Map Text Text
    , _aceConfigBasePath  :: Maybe Text
    , _aceConfigMode      :: Maybe Text
    , _aceConfigTheme     :: Maybe AceTheme
    }

instance Default AceConfig where
    def = AceConfig "editor" def def def def

newtype AceRef = AceRef { unAceRef :: JSVal }

data ACE t = ACE
    { aceRef   :: Dynamic t (Maybe AceRef)
    , aceValue :: Dynamic t Text
    }

mtext2val :: Maybe Text -> JSVal
mtext2val = maybe jsNull (jsval . toJSString)

------------------------------------------------------------------------------
startACE
    :: AceConfig
    -> IO AceRef
#ifdef ghcjs_HOST_OS
startACE ac =
    js_startACE (toJSString $ _aceConfigElemId ac)
                (mtext2val $ _aceConfigBasePath ac)
                (mtext2val $ _aceConfigMode ac)
                (mtext2val $ T.pack . show <$> _aceConfigTheme ac)

foreign import javascript unsafe
  "(function(){\
     if ($2) ace['config']['set']('basePath', $2);\
     var a = ace['edit']($1);\
     if ($3) a['session']['setMode']($3);\
     if ($4) a['setTheme']($4);\
     return a; })()"
  js_startACE :: JSString -> JSVal -> JSVal -> JSVal -> IO AceRef

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
    elAttr "div" ("id" =: _aceConfigElemId ac <> _aceConfigElemAttrs ac) $
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


