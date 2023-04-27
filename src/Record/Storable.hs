{-# LANGUAGE 
      GADTs
    , TypeFamilies
    , PolyKinds
    , DataKinds
    , AllowAmbiguousTypes
    , UndecidableInstances
#-}

module Record.Storable (
    -- * Record field
      (:=)(..)
    , FldProxy(..)
    -- * Immutable record
    , Rec
    -- ** Record construction
    , record
    -- ** Accessing elements
    , lens
    , getFields
    , getField
    , GetField
    , setField
    , SetField
    , modifyField
    -- ** Conversion to/from mutable record
    , freeze
    , thaw
    , unsafeFreeze
    , unsafeThaw
    , copy
    -- * Re-exports
    -- ** Heterogenous list
    , HList(..)
    -- ** Record info
    , RecSize
    , Layout
    -- ** Accessing elements
    , ReadFields
    , WriteFields
    -- ** Label info
    , LabelIndex
    , LabelType
    , LabelLayout
    , LabelOffset
    , LabelSize
) where

import Control.Monad.Primitive
import Data.Kind
import Prelude.Singletons
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Promoted
import Record.Storable.Mutable
import GHC.TypeLits

---------------------------------------------------------------------------------------------------
-- | Immutable anonymous record.
newtype Rec (ts :: [Type]) = Rec { _recPtr :: ForeignPtr () }

instance (ReadFields ts, Eq (HList ts)) => Eq (Rec ts) where
    a == b = getFields a == getFields b
    {-# INLINE (==) #-}

instance (ReadFields ts, Show (HList ts)) => Show (Rec ts) where
    show r = "Rec " ++ show (getFields r)
    {-# INLINE show #-}

instance PStorable (Rec ts) where
    type SizeOf    (Rec ts) = RecSize ts
    type Alignment (Rec ts) = RecAlignment ts

instance ( KnownNat (RecSize ts)
         , KnownNat (Alignment (Rec ts))
         ) => Storable (Rec ts)
    where
    {-# INLINE sizeOf    #-}
    {-# INLINE alignment #-}
    {-# INLINE peek      #-}
    {-# INLINE poke      #-}

    sizeOf _ = demoteInt @(SizeOf (Rec ts))
    alignment _ = demoteInt @(Alignment (Rec ts))

    peek p = do
        mr@(MRec mfp) <- newMRec @ts
        withForeignPtr mfp $ \mp ->
            copyBytes mp (castPtr p) (demoteInt @(RecSize ts))
        unsafeFreeze mr

    poke dest (Rec fp) = do
        withForeignPtr fp $ \p ->
            copyBytes dest (castPtr p) (demoteInt @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Create a record from given 'HList' of fields.
record :: forall (ts :: [Type]).
       ( KnownNat (RecSize ts)
       , WriteFields ts
       )
       => HList ts
       -> Rec ts
record hl = 
    unsafeInlineIO $ unsafeFreeze =<< mrecord hl
{-# INLINE record #-}

---------------------------------------------------------------------------------------------------
-- | Simple lens.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Lens for a field.
lens :: forall (l :: Symbol) (ts :: [Type]).
     ( GetField l ts
     , SetField l ts
     )
     => FldProxy l 
     -> Lens' (Rec ts) (LabelType l ts)
lens l f r = fmap (setField l r) (f (getField l r))
{-# INLINE lens #-}

---------------------------------------------------------------------------------------------------
-- | Get the fields of the record in form of a 'HList'.
getFields :: forall (ts :: [Type]).
          ( ReadFields ts )
          => Rec ts
          -> HList ts
getFields r = unsafeInlineIO $ do
    mr <- unsafeThaw r
    readFields mr
{-# INLINE getFields #-}

---------------------------------------------------------------------------------------------------
-- | Get the field @l@ of the record.
getField :: forall (l :: Symbol) (ts :: [Type]).
         ( GetField l ts )
         => FldProxy l
         -> Rec ts
         -> LabelType l ts
getField _ r = unsafeInlineIO $ do
    mr <- unsafeThaw r
    readField @l @ts FldProxy mr
{-# INLINE getField #-}

-- | Constraints for 'getField'.
type GetField (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , KnownNat (LabelOffset l ts)
    )

-- | Yield a copy of the record with the field @l@ set to given value.
setField :: forall (l :: Symbol) (ts :: [Type]).
         ( SetField l ts )
         => FldProxy l
         -> Rec ts
         -> LabelType l ts
         -> Rec ts
setField _ r v = unsafeInlineIO $ do
    mr <- thaw r
    writeField @l @ts FldProxy mr v
    unsafeFreeze mr
{-# INLINE setField #-}

-- | Constraints for 'setField'.
type SetField (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , KnownNat (RecSize ts)
    , KnownNat (LabelOffset l ts)
    )

-- | Yield a copy of the record with the field @l@ modified by given function.
modifyField :: forall (l :: Symbol) (ts :: [Type]).
            ( ModifyField l ts )
            => FldProxy l
            -> Rec ts
            -> (LabelType l ts -> LabelType l ts)
            -> Rec ts
modifyField _ r f = unsafeInlineIO $ do
    mr <- thaw r
    v <- readField @l @ts FldProxy mr
    writeField @l @ts FldProxy mr (f v)
    unsafeFreeze mr
{-# INLINE modifyField #-}

-- | Constraints for 'setField'.
type ModifyField (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , KnownNat (RecSize ts)
    , KnownNat (LabelOffset l ts)
    )

---------------------------------------------------------------------------------------------------
-- | Yield an immutable copy of the mutable record.
freeze :: forall (ts :: [Type]) m.
       ( PrimMonad m
       , KnownNat (RecSize ts)
       )
       => MRec (PrimState m) ts
       -> m (Rec ts)
freeze mr = unsafeFreeze =<< clone mr
{-# INLINE freeze #-}

-- | Yield a mutable copy of the immutable record.
thaw :: forall (ts :: [Type]) m.
     ( PrimMonad m
     , KnownNat (RecSize ts)
     )
     => Rec ts
     -> m (MRec (PrimState m) ts)
thaw r = do
    mr <- newMRec @ts
    copy r mr
    pure mr
{-# INLINE thaw #-}

-- | Unsafely convert a mutable record to an immutable one without copying.
--   The mutable record may not be used after this operation.
unsafeFreeze :: forall (ts :: [Type]) m.
             (PrimMonad m)
             => MRec (PrimState m) ts
             -> m (Rec ts)
unsafeFreeze (MRec fp) = pure $ Rec fp
{-# INLINE unsafeFreeze #-}

-- | Unsafely convert an immutable record to a mutable one without copying.
--   The immutable record may not be used after this operation.
unsafeThaw :: forall (ts :: [Type]) m.
           (PrimMonad m)
           => Rec ts
           -> m (MRec (PrimState m) ts)
unsafeThaw (Rec fp) = pure $ MRec fp
{-# INLINE unsafeThaw #-}

-- | Copy an immutable record into a mutable one. 
copy :: forall (ts :: [Type]) m.
     ( KnownNat (RecSize ts)
     , PrimMonad m
     )
     => Rec ts
     -> MRec (PrimState m) ts
     -> m ()
copy (Rec fp) (MRec mfp) = do
    unsafePrimToPrim $ withForeignPtr fp $ \p ->
        withForeignPtr mfp $ \mp ->
            copyBytes mp p (demoteInt @(RecSize ts))
{-# INLINE copy #-}
