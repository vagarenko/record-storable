{-# LANGUAGE
      DataKinds
    , ScopedTypeVariables
    , KindSignatures
    , TypeInType
    , RankNTypes
    , AllowAmbiguousTypes
    , TypeApplications
    , MultiParamTypeClasses
    , UndecidableInstances
#-}

module Foreign.Storable.Promoted.TH where

import Foreign.Storable
import Language.Haskell.TH
import qualified Data.Kind as K

-- | Size of @t@ as type-level 'Nat' literal.
pSizeOf :: forall (t :: K.Type). (Storable t) => TypeQ
pSizeOf = pure $ LitT $ NumTyLit $ fromIntegral $ sizeOf (undefined :: t)

-- | Alignment of @t@ as type-level 'Nat' literal.
pAlignment :: forall (t :: K.Type). (Storable t) => TypeQ
pAlignment = pure $ LitT $ NumTyLit $ fromIntegral $ alignment (undefined :: t)
