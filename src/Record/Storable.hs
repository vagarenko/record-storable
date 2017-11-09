{-# LANGUAGE 
      FlexibleInstances
    , FlexibleContexts
    , DataKinds
    , GADTs
    , TypeOperators
    , ScopedTypeVariables
    , KindSignatures
    , TypeFamilies
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
    , StandaloneDeriving
#-}

module Record.Storable where

import Control.Monad.Primitive
import Data.Kind
import Data.Singletons.Prelude               hiding (type (+), type (-))
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Promoted
import Record.Storable.Mutable

--foo :: Rec '["a" := Int, "b" := Float, "c" := Double, "d" := Int, "e" := Float]
-- foo = do
--     r <- newMRec @'["a" := Int, "b" := Float, "c" := Double, "d" := Int, "e" := Float] @IO
--     writeField #a r (0 :: Int)
--     writeField #b r (0 :: Float)
--     writeField #c r (0 :: Double)
--     writeField #d r (0 :: Int)
--     writeField #e r (0 :: Float) 
--     --pure r
foo = record
    $  #a := (0 :: Int)
    :& #b := (0 :: Float)
    :& #c := (0 :: Double)
    :& #d := (0 :: Int)
    :& #e := (record
        $  #a := (0 :: Int)
        :& #b := (0 :: Float)
        :& #c := (0 :: Double)
        :& #d := (record
            $  #a := (0 :: Int)
            :& #b := (0 :: Float)
            :& #c := (0 :: Double)
            :& #d := (0 :: Int)
            :& Nil)
        :& Nil)
    :& Nil

---------------------------------------------------------------------------------------------------
-- | Immutable anonymous record.
newtype Rec (ts :: [Type]) = Rec { _recPtr :: (ForeignPtr ()) }

instance (ReadFields ts, Eq (HList ts)) => Eq (Rec ts) where
    a == b = getFields a == getFields b
    {-# INLINE (==) #-}

instance (ReadFields ts, Show (HList ts)) => Show (Rec ts) where
    show r = "Rec " ++ show (getFields r)
    {-# INLINE show #-}

type instance SizeOf (Rec ts) = RecSize ts

type instance Alignment (Rec '[]      ) = 1
type instance Alignment (Rec (t ': ts)) = Alignment t `Max` Alignment (Rec ts)

instance ( NatVal (RecSize ts)
         , NatVal (Alignment (Rec ts))
         ) => Storable (Rec ts)
    where
    sizeOf _ = natVal @(SizeOf (Rec ts))
    alignment _ = natVal @(Alignment (Rec ts))

    peek p = do
        mr@(MRec mfp) <- newMRec @ts
        withForeignPtr mfp $ \mp ->
            copyBytes mp (castPtr p) (natVal @(RecSize ts))
        unsafeFreeze mr

    poke dest (Rec fp) = do
        withForeignPtr fp $ \p ->
            copyBytes dest (castPtr p) (natVal @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Create a record from given 'HList' of fields.
record :: forall (ts :: [Type]).
       ( NatVal (RecSize ts)
       , WriteFields ts
       )
       => HList ts
       -> Rec ts
record hl = 
    unsafeInlineIO $ unsafeFreeze =<< mrecord hl
{-# INLINE record #-}

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
         ( Storable (LabelType l ts)
         , NatVal (LabelOffset l ts)
         )
         => FldProxy l
         -> Rec ts
         -> LabelType l ts
getField _ r = unsafeInlineIO $ do
    mr <- unsafeThaw r
    readField @l @ts FldProxy mr
{-# INLINE getField #-}

-- | Yield a copy of the record with the field @l@ set to given value.
setField :: forall (l :: Symbol) (ts :: [Type]).
         ( Storable (LabelType l ts)
         , NatVal (RecSize ts)
         , NatVal (LabelOffset l ts)
         )
         => FldProxy l
         -> Rec ts
         -> LabelType l ts
         -> Rec ts
setField _ r v = unsafeInlineIO $ do
    mr <- thaw r
    writeField @l @ts FldProxy mr v
    unsafeFreeze mr
{-# INLINE setField #-}

-- | Yield a copy of the record with the field @l@ modified by given function.
modifyField :: forall (l :: Symbol) (ts :: [Type]).
            ( Storable (LabelType l ts)
            , NatVal (RecSize ts)
            , NatVal (LabelOffset l ts)
            )
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

---------------------------------------------------------------------------------------------------
-- | Yield an immutable copy of the mutable record.
freeze :: forall (ts :: [Type]) m.
       ( PrimMonad m
       , NatVal (RecSize ts)
       )
       => MRec (PrimState m) ts
       -> m (Rec ts)
freeze mr = unsafeFreeze =<< clone mr
{-# INLINE freeze #-}

-- | Yield a mutable copy of the immutable record.
thaw :: forall (ts :: [Type]) m.
     ( PrimMonad m
     , NatVal (RecSize ts)
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
     ( NatVal (RecSize ts)
     , PrimMonad m
     )
     => Rec ts
     -> MRec (PrimState m) ts
     -> m ()
copy (Rec fp) (MRec mfp) = do
    unsafePrimToPrim $ withForeignPtr fp $ \p ->
        withForeignPtr mfp $ \mp ->
            copyBytes mp p (natVal @(RecSize ts))
{-# INLINE copy #-}
