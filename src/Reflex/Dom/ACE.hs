{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|

Basic support for using the ACE editor with Reflex.

Example usage:

    ace <- divClass "yourACEWrapperDiv" $ -- wrapper div not required
      aceWidget def (AceDynConfig Nothing) never "initial editor contents"

    -- The rest is optional and lets you change what's in the editor on the fly
    -- fly without redrawing the widget.
    withAceInstance ace (setValueACE <$> updatesToContents)
    holdDyn iv $ leftmost
      [ updatesToContents
      , updated (aceValue ace)
      ]

-}

module Reflex.Dom.ACE where

------------------------------------------------------------------------------
import           Control.Lens                       ((^.))
import           Control.Monad                      (unless, void)
import           Control.Monad.Trans
import           Data.Default
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           GHCJS.DOM.Types                    (Element, JSVal, toJSString)
import           Language.Javascript.JSaddle        (asyncFunction,
                                                     fromJSValUnchecked, js,
                                                     js0, js1, js2, jsg, jsval,
                                                     pToJSVal)
import           Language.Javascript.JSaddle.Object (MakeObject (..), create,
                                                     (<#))
import           Language.Javascript.JSaddle.Types  (JSM, MonadJSM, ghcjsPure,
                                                     liftJSM)
import           Language.Javascript.JSaddle.Value  (ToJSVal (..), jsNull)
import           Reflex
import           Reflex.Dom                         hiding (Element,
                                                     fromJSString)
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
  | AceTheme_SolarizedDark
  | AceTheme_Terminal
  | AceTheme_TomorrowNight
  | AceTheme_TomorrowNightBlue
  | AceTheme_TomorrowNightBright
  | AceTheme_TomorrowNightEighties
  | AceTheme_Twilight
  | AceTheme_VibrantInk
  deriving (Eq,Ord,Enum,Bounded)

instance Show AceTheme where
    show AceTheme_Ambiance              = "ambiance"
    show AceTheme_Chaos                 = "chaos"
    show AceTheme_Chrome                = "chrome"
    show AceTheme_Clouds                = "clouds"
    show AceTheme_CloudsMidnight        = "clouds_midnight"
    show AceTheme_Cobalt                = "cobalt"
    show AceTheme_CrimsonEditor         = "crimson_editor"
    show AceTheme_Dawn                  = "dawn"
    show AceTheme_Dreamweaver           = "dreamweaver"
    show AceTheme_Eclipse               = "eclipse"
    show AceTheme_Github                = "github"
    show AceTheme_Gruvbox               = "gruvbox"
    show AceTheme_IdleFingers           = "idle_fingers"
    show AceTheme_Iplastic              = "iplastic"
    show AceTheme_Katzenmilch           = "katzenmilch"
    show AceTheme_KrTheme               = "kr_theme"
    show AceTheme_Kuroir                = "kuroir"
    show AceTheme_Merbivore             = "merbivore"
    show AceTheme_MerbivoreSoft         = "merbivore_soft"
    show AceTheme_MonoIndustrial        = "mono_industrial"
    show AceTheme_Monokai               = "monokai"
    show AceTheme_PastelOnDark          = "pastel_on_dark"
    show AceTheme_SolarizedDark         = "solarized_dark"
    show AceTheme_SolarizedLight        = "solarized_light"
    show AceTheme_Sqlserver             = "sqlserver"
    show AceTheme_Terminal              = "terminal"
    show AceTheme_Textmate              = "textmate"
    show AceTheme_Tomorrow              = "tomorrow"
    show AceTheme_TomorrowNight         = "tomorrow_night"
    show AceTheme_TomorrowNightBlue     = "tomorrow_night_blue"
    show AceTheme_TomorrowNightBright   = "tomorrow_night_bright"
    show AceTheme_TomorrowNightEighties = "tomorrow_night_eighties"
    show AceTheme_Twilight              = "twilight"
    show AceTheme_VibrantInk            = "vibrant_ink"
    show AceTheme_Xcode                 = "xcode"


data AceConfig = AceConfig
    { _aceConfigElemAttrs       :: Map Text Text
    , _aceConfigBasePath        :: Maybe Text
    , _aceConfigMode            :: Maybe Text
    , _aceConfigWordWrap        :: Bool
    , _aceConfigShowPrintMargin :: Bool
    , _aceConfigReadOnly        :: Bool
    }


data AceDynConfig = AceDynConfig
    { _aceDynConfigTheme :: Maybe AceTheme
    }


instance Default AceConfig where
    def = AceConfig def def def False False False


newtype AceInstance = AceInstance { unAceInstance :: JSVal }


data ACE t = ACE
    { aceRef   :: Dynamic t (Maybe AceInstance)
    , aceValue :: Dynamic t Text
    }


------------------------------------------------------------------------------
-- The type of editor session line annotation.
data AnnotationType = AnnotationError
                    | AnnotationWarning
                    deriving (Show, Read)

------------------------------------------------------------------------------
instance ToJSVal AnnotationType where
  toJSVal AnnotationError   = toJSVal "error"
  toJSVal AnnotationWarning = toJSVal "warning"


------------------------------------------------------------------------------
-- A line annotation for marking a specific line within the editor session as
-- an error or a warning.
data Annotation = Annotation { annotationRow    :: Int
                             , annotationColumn :: Int
                             , annotationText   :: Text
                             , annotationType   :: AnnotationType
                             } deriving (Show, Read)


------------------------------------------------------------------------------
instance MakeObject Annotation where
  makeObject (Annotation row col txt typ) = do
    o <- create
    (o <# "row"   ) row
    (o <# "column") col
    (o <# "text"  ) txt
    (o <# "type"  ) typ
    return o


instance ToJSVal Annotation where
  toJSVal = (toJSVal =<<) . makeObject


------------------------------------------------------------------------------
mtext2val :: Maybe Text -> JSM JSVal
mtext2val = maybe (pure jsNull) (ghcjsPure . jsval . toJSString)


------------------------------------------------------------------------------
startACE :: MonadJSM m => Element -> AceConfig -> m AceInstance
startACE elmt ac = liftJSM $ do
  aceVal <- jsg "ace"
  let [basePath, mode] = map (fromMaybe (T.pack "") . ($ ac))
                              [_aceConfigBasePath, _aceConfigMode]
  -- Set the base path if given
  unless (T.null basePath) $ do
    config <- aceVal ^. js "config"
    void $ config ^. js2 "set" "basePath" basePath
  -- Start and return an editing session
  editSession <- aceVal ^. js1 "edit" (pToJSVal elmt)
  -- Set the mode if given
  unless (T.null mode) $ do
    session <- editSession ^. js "session"
    void $ session ^. js1 "setMode" mode
  -- Set readOnly
  void $ editSession ^. js1 "setReadOnly" (_aceConfigReadOnly ac)
  let aceInst  = AceInstance editSession
  setUseWrapMode (_aceConfigWordWrap ac) aceInst
  setShowPrintMargin (_aceConfigShowPrintMargin ac) aceInst
  return aceInst


------------------------------------------------------------------------------
moveCursorToPosition :: MonadJSM m => (Int, Int) -> AceInstance -> m ()
moveCursorToPosition (r, c) (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 "gotoLine" r c


------------------------------------------------------------------------------
setThemeACE :: MonadJSM m => Maybe AceTheme -> AceInstance -> m ()
setThemeACE Nothing      _                 = return ()
setThemeACE (Just theme) (AceInstance ace) =
  liftJSM $ void $ ace ^. js1 "setTheme" themeStr
  where themeStr = "ace/theme/" <> show theme


------------------------------------------------------------------------------
setModeACE :: MonadJSM m => Text -> AceInstance -> m ()
setModeACE mode (AceInstance ace) = liftJSM $ do
  session <- ace ^. js "session"
  void $ session ^. js1 "setMode" mode


------------------------------------------------------------------------------
setReadOnlyACE :: MonadJSM m => Bool -> AceInstance -> m ()
setReadOnlyACE readOnly (AceInstance ace) =
  liftJSM $ void $ ace ^. js1 "setReadOnly" readOnly


------------------------------------------------------------------------------
setUseWrapMode :: MonadJSM m => Bool -> AceInstance -> m ()
setUseWrapMode shouldWrap (AceInstance ace) = liftJSM $ do
  session <- ace ^. js0 "getSession"
  void $ session ^. js1 "setUseWrapMode" shouldWrap


------------------------------------------------------------------------------
setShowPrintMargin :: MonadJSM m => Bool -> AceInstance -> m ()
setShowPrintMargin shouldShow (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 "setOption" "showPrintMargin" shouldShow


------------------------------------------------------------------------------
setUseWorker :: MonadJSM m => Bool -> AceInstance -> m ()
setUseWorker shouldUse (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 "setOption" "useWorker" shouldUse


------------------------------------------------------------------------------
setAnnotations :: MonadJSM m => [Annotation] -> AceInstance -> m ()
setAnnotations as (AceInstance ace) = liftJSM $ do
  session <- ace ^. js0 "getSession"
  annotations <- toJSValListOf as
  void $ session ^. js1 "setAnnotations" annotations


------------------------------------------------------------------------------
setConfigACE :: MonadJSM m => Text -> Text -> AceInstance -> m ()
setConfigACE t1 t2 (AceInstance ace) = liftJSM $ do
  cfg <- ace ^. js "config"
  void $ cfg ^. js2 "set" t1 t2


------------------------------------------------------------------------------
getValueACE :: MonadJSM m => AceInstance -> m Text
getValueACE (AceInstance ace) =
  liftJSM $ ace ^. js0 "getValue" >>= fromJSValUnchecked


------------------------------------------------------------------------------
setValueACE :: MonadJSM m => Text -> AceInstance -> m ()
setValueACE t (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 "setValue" t (-1 :: Int)


------------------------------------------------------------------------------
setupValueListener
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     )
  => AceInstance
  -> m (Event t Text)
setupValueListener (AceInstance ace) = do
  pb  <- getPostBuild
  let act cb = liftJSM $ do
        jscb <- asyncFunction $ \_ _ _ ->
          getValueACE (AceInstance ace) >>= liftIO . cb
        void $ ace ^. js2 "on" "change" jscb
  performEventAsync (act <$ pb)


------------------------------------------------------------------------------
-- | Main entry point
aceWidget
    :: MonadWidget t m
    => AceConfig -> AceDynConfig -> Event t AceDynConfig -> Text -> m (ACE t)
aceWidget ac adc adcUps initContents = do
    attrs <- holdDyn (addThemeAttr adc) (addThemeAttr <$> adcUps)
    aceDiv <- fmap fst $ elDynAttr' (T.pack "div") attrs $ text initContents
    aceInstance <- startACE (_element_raw aceDiv) ac
    onChange <- setupValueListener aceInstance
    updatesDyn <- holdDyn initContents onChange

    let ace = ACE (constDyn $ pure aceInstance) updatesDyn
    setThemeACE (_aceDynConfigTheme adc) aceInstance
    withAceInstance ace (setThemeACE . _aceDynConfigTheme <$> adcUps)
    return ace
  where
    static = _aceConfigElemAttrs ac
    themeAttr t = T.pack $ " ace-" <> show t
    addThemeAttr c = maybe static
      (\t -> M.insertWith (<>) (T.pack "class") (themeAttr t) static)
      (_aceDynConfigTheme c)


------------------------------------------------------------------------------
-- | This function is the same a aceWidget except it uses elAttr' instead of
-- elDynAttr' which for some unexplained reason solves editor rendering
-- problems in some situations.
--
-- We're adding this as a separate function to avoid potentially breaking
-- users that may have been depending on the old behavior.  This function may
-- replace aceWidget in the future and go away.
aceWidgetStatic
    :: MonadWidget t m
    => AceConfig -> AceDynConfig -> Text -> m (ACE t)
aceWidgetStatic ac adc initContents = do
    aceDiv <- fmap fst $ elAttr' (T.pack "div") (addThemeAttr adc) $ text initContents
    aceInstance <- startACE (_element_raw aceDiv) ac
    onChange <- setupValueListener aceInstance
    updatesDyn <- holdDyn initContents onChange

    let ace = ACE (constDyn $ pure aceInstance) updatesDyn
    setThemeACE (_aceDynConfigTheme adc) aceInstance
    return ace
  where
    static = _aceConfigElemAttrs ac
    themeAttr t = T.pack $ " ace-" <> show t
    addThemeAttr c = maybe static
      (\t -> M.insertWith (<>) (T.pack "class") (themeAttr t) static)
      (_aceDynConfigTheme c)


------------------------------------------------------------------------------
-- | Convenient helper function for running functions that need an AceInstance.
withAceInstance
    :: PerformEvent t m
    => ACE t
    -> Event t (AceInstance -> Performable m ())
    -> m (Event t ())
withAceInstance ace evt = withAceInstance' ace (f <$> evt)
  where
    f _ Nothing  = return ()
    f g (Just a) = g a


------------------------------------------------------------------------------
-- | More powerful function for running functions that need an AceInstance.
withAceInstance'
    :: PerformEvent t m
    => ACE t
    -> Event t (Maybe AceInstance -> Performable m a)
    -> m (Event t a)
withAceInstance' ace =
  performEvent . attachPromptlyDynWith (flip ($)) (aceRef ace)
