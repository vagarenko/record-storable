{-# LANGUAGE 
      FlexibleInstances
    , FlexibleContexts
    , DataKinds
    , GADTs
    , TypeOperators
    , ScopedTypeVariables
    , KindSignatures
    , TypeFamilies
    , NoMonoLocalBinds
    , TypeInType
    , RankNTypes
    , AllowAmbiguousTypes
    , UnboxedTuples
    , ConstraintKinds
    , InstanceSigs
    , TypeApplications 
    , MultiParamTypeClasses
    , UndecidableInstances
    , OverloadedLabels
    , TemplateHaskell
#-}

module Foreign.Storable.Promoted.TH where

import Foreign.Storable
import Language.Haskell.TH
import qualified Data.Kind as K

pSizeOf :: forall (t :: K.Type). (Storable t) => TypeQ
pSizeOf = pure $ LitT $ NumTyLit $ fromIntegral $ sizeOf (undefined :: t)

pAlignment :: forall (t :: K.Type). (Storable t) => TypeQ
pAlignment = pure $ LitT $ NumTyLit $ fromIntegral $ alignment (undefined :: t)
