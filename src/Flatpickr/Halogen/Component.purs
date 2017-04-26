module Flatpickr.Halogen.Component
  ( component
  , Query(..)
  , Message(..)
  , Input
  , Effects
  ) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (head)
import Data.Foldable (for_)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Flatpickr (flatpickr, setDate, onChange, onClose, destroy, open) as FP
import Flatpickr.Config (defaultConfig) as FP
import Flatpickr.Types (FLATPICKR)
import Flatpickr.Types (DateType(DateJSDate), Flatpickr) as FP
import Halogen.Query.HalogenM (halt)

type Effects eff =
   ( flatpickr :: FLATPICKR
   , avar :: AVAR
   | eff
   )

data Query a
  = Initialize a
  | Finalize a
  | Update Input a
  | Open a
  | HandleChange (Array JSDate) (H.SubscribeStatus -> a)
  | HandleClose (H.SubscribeStatus -> a)

type State =
  { input :: Input
  , flatpickr :: Maybe FP.Flatpickr
  }

type Input =
  { date :: JSDate
  }

data Message
  = DateChanged JSDate
  | Closed

component
  :: forall eff m. MonadAff (Effects eff) m
  => H.Component HH.HTML Query Input Message m
component = H.lifecycleComponent
  { initialState:
      { input: _
      , flatpickr: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

render :: State -> H.ComponentHTML Query
render _ = HH.input
  [ HP.placeholder "Select date..."
  , HP.type_ HP.InputText
  , HP.ref (H.RefLabel "flatpickr")
  ]

eval
  :: forall eff m. MonadAff (Effects eff) m
  => Query ~> H.ComponentDSL State Query Message m
eval (Initialize next) = do
  H.getHTMLElementRef (H.RefLabel "flatpickr") >>= case _ of
    Nothing -> halt "Flatpickr: Could not find elemet to attach to."
    Just el -> do
      jsdate <- H.gets _.input.date
      flatpickr <- liftEff do
        let
          config = FP.defaultConfig
            { defaultDate = FP.DateJSDate jsdate
            }
        FP.flatpickr el config
      H.modify _
        { flatpickr = Just flatpickr }
      H.subscribe $ H.eventSource
        (\call -> FP.onChange flatpickr \dates _ _ -> call dates)
        (Just <<< H.request <<< HandleChange)
      H.subscribe $ H.eventSource_
        (\call -> FP.onClose flatpickr \dates _ _ -> call)
        (H.request HandleClose)
  pure next

eval (Finalize next) = do
  mPicker <- H.gets _.flatpickr
  case mPicker of
    Nothing -> halt "Flatpickr finalize: not properly initialized."
    Just flatpickr ->
      liftEff $ FP.destroy flatpickr
  pure next

eval (Update input next) = do
  mPicker <- H.gets _.flatpickr
  _ <- case mPicker of
    Nothing -> halt "Flatpickr update: not properly initialized."
    Just flatpickr ->
      liftEff $ FP.setDate (FP.DateJSDate input.date) false flatpickr
  pure next

eval (Open next) = do
  mPicker <- H.gets _.flatpickr
  for_ mPicker \picker ->
    liftEff $ FP.open picker
  pure next

eval (HandleChange dates reply) = do
  case head dates of
    Nothing -> pure unit
    Just jsdate -> do
      H.raise $ DateChanged jsdate
  pure $ reply H.Listening

eval (HandleClose reply) = do
  H.raise Closed
  pure $ reply H.Listening
