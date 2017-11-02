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

module Record.Storable.Mutable where

import Control.Monad.Primitive
import Data.Kind
import Data.Singletons.Prelude               hiding (type (+), type (-))
import Data.Singletons.TypeLits              (Mod)
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Promoted
import GHC.OverloadedLabels
import GHC.Types
import GHC.TypeLits
import Language.Haskell.TH                   hiding (Type)

---------------------------------------------------------------------------------------------------
-- | Record field.
data label := value =
    KnownSymbol label => FldProxy label := !value
infix 6 :=

instance (Eq value) => Eq (label := value) where
  (_ := x) == (_ := y) = x == y
  {-# INLINE (==) #-}

instance (Ord value) => Ord (label := value) where
  compare (_ := x) (_ := y) = x `compare` y
  {-# INLINE compare #-}

instance (Show t) =>
         Show (l := t) where
  showsPrec p (l := t) =
      showParen (p > 10) (showString ("#" ++ symbolVal l ++ " := " ++ show t))

instance (Storable v, KnownSymbol l) => Storable (l := v) where
    {-# INLINE alignment #-}
    {-# INLINE sizeOf #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}

    sizeOf _        = sizeOf (undefined :: v)
    alignment _     = alignment (undefined :: v)
    peek p          = (FldProxy @l :=) <$> peek (castPtr p)
    poke p (_ := v) = poke (castPtr p) v

type instance SizeOf (l := v) = SizeOf v
type instance Alignment (l := v) = Alignment v

---------------------------------------------------------------------------------------------------
-- | A 'Proxy' restricted to 'Symbol' kind.
data FldProxy (t :: Symbol) = FldProxy
    deriving (Show, Read, Eq, Ord)

instance (l ~ l') => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel = FldProxy

---------------------------------------------------------------------------------------------------
-- |
data Rec (ts :: [Type]) = Rec { _recPtr :: !(ForeignPtr ()) }


type instance SizeOf (Rec ts) = RecSize ts

type instance Alignment (Rec '[]      ) = 1
type instance Alignment (Rec (t ': ts)) = Alignment t `Max` Alignment (Rec ts)

---------------------------------------------------------------------------------------------------
-- | Create a record from 'HList' of fields.
record :: forall (ts :: [Type]) (m :: Type -> Type).
       ( NatVal_ (RecSize ts)
       , WriteFields ts ts
       , PrimMonad m
       )
       => HList ts
       -> m (Rec ts)
record hl = do
    r <- mallocRec @ts
    writeFields @ts r hl
    pure r

rrr :: IO (Rec '["a" := Int, "b" := Float, "c" := Double])
rrr = record
    $  #a := (0 :: Int)
    :& #b := (0.1 :: Float)
    :& #c := (0.00000000001 :: Double)
    :& Nil

-- | Make copy of the record.
copy :: forall (ts :: [Type]) m.
     ( NatVal_ (RecSize ts)
     , PrimMonad m
     )
     => Rec ts
     -> m (Rec ts)
copy (Rec fp0) = do
    r1@(Rec fp1) <- mallocRec @ts
    unsafePrimToPrim $ withForeignPtr fp0 $ \p0 ->
        withForeignPtr fp1 $ \p1 ->
            copyBytes p1 p0 (natVal_ @(RecSize ts))
    pure r1

---------------------------------------------------------------------------------------------------
-- | Heterogenous list.
data HList (ts :: [Type]) where
    (:&) :: t -> HList ts -> HList (t ': ts)
    Nil  :: HList '[]

infixr 5 :&

-------------------------------------------------
instance Eq (HList '[]) where
    Nil == Nil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :& as) == (b :& bs) = a == b && as == bs

-------------------------------------------------
instance (ShowHList ts) => Show (HList ts) where
    show = show . showHList

class ShowHList (ts :: [Type]) where
    showHList :: HList ts -> [String]

instance ShowHList '[] where
    showHList Nil = []

instance (Show t, ShowHList ts) => ShowHList (t ': ts) where
    showHList (a :& as) = show a : showHList as

---------------------------------------------------------------------------------------------------
-- | Size of the record in bytes.
type family RecSize (ts :: [Type]) :: Nat where
    RecSize ts = Fst (Snd (Last (Layout ts))) + Snd (Snd (Last (Layout ts)))

-- | List of offsets and sizes of each label.
type family Layout (ts :: [Type]) :: [(Symbol, (Nat, Nat))] where
    Layout ts = LayoutWrk ts 0

type family LayoutWrk (ts :: [Type]) (sizeAcc :: Nat) :: [(Symbol, (Nat, Nat))] where
    LayoutWrk '[]              _       = '[]
    LayoutWrk ((l := v) ': ts) sizeAcc = '(l, '(LayoutWrkOffset v sizeAcc, SizeOf v)) ': LayoutWrk ts (LayoutWrkOffset v sizeAcc + SizeOf v)

type family LayoutWrkOffset (t :: Type) (sizeAcc :: Nat) :: Nat where
    LayoutWrkOffset t sizeAcc = sizeAcc + LayoutWrkPadding t sizeAcc

type family LayoutWrkPadding (t :: Type) (sizeAcc :: Nat) :: Nat where
    LayoutWrkPadding t sizeAcc = Mod (Diff (Alignment t) sizeAcc) (Alignment t)
--  layoutWrk sizeAcc = (offset, size) : layoutWrk ts sizeAcc'
--      where
--          offset   = sizeAcc + padding
--          padding  = (alignment t - sizeAcc) `mod` alignment t
--          sizeAcc' = offset + sizeOf t

-- | Absolute value of a difference between two nats.
type family Diff (a :: Nat) (b :: Nat) :: Nat where
    Diff a b = DiffWrk a b (a <=? b)

type family DiffWrk (a :: Nat) (b :: Nat) (a_lte_b :: Bool) :: Nat where
    DiffWrk a b 'True  = b - a
    DiffWrk a b 'False = a - b

---------------------------------------------------------------------------------------------------
-- | Allocate memory for the record. The memory is not initialized.
mallocRec :: forall (ts :: [Type]) m.
          ( NatVal_ (RecSize ts)
          , PrimMonad m
          ) 
          => m (Rec ts)
mallocRec = unsafePrimToPrim $ Rec <$> mallocForeignPtrBytes (natVal_ @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Read fields from record.
readFields :: forall (ts :: [Type]) m.
           ( ReadFields ts ts
           , PrimMonad m
           )
           => Rec ts
           -> m (HList ts)
readFields = readFieldsWrk @ts @ts

class ReadFields (hts :: [Type]) (rts :: [Type]) where
    readFieldsWrk :: (PrimMonad m) => Rec rts -> m (HList hts)

instance ReadFields '[] rts where
    readFieldsWrk _ = pure Nil
    {-# INLINE readFieldsWrk #-}

instance ( ReadFields hts rts
         , ReadFieldCtx l rts
         , KnownSymbol l
         , LabelType l rts ~ v
         ) => ReadFields ((l := v) ': hts) rts where
    readFieldsWrk r = do
        v <- readField @l r
        (FldProxy @l := v :&) <$> readFieldsWrk @hts r
    {-# INLINE readFieldsWrk #-}

-------------------------------------------------
-- | Write fields into record.
writeFields :: forall (ts :: [Type]) (m :: Type -> Type).
            ( WriteFields ts ts
            , PrimMonad m
            )
            => Rec ts
            -> HList ts
            -> m ()
writeFields = writeFieldsWrk @ts @ts

class WriteFields (hts :: [Type]) (rts :: [Type]) where
    writeFieldsWrk :: (PrimMonad m) => Rec rts -> HList hts -> m ()

instance WriteFields '[] rts where
    writeFieldsWrk _ _ = pure ()
    {-# INLINE writeFieldsWrk #-}

instance ( WriteFields hts rts
         , WriteFieldCtx l rts
         , LabelType l rts ~ v
         ) => WriteFields ((l := v) ': hts) rts
    where
    writeFieldsWrk r ((_l := v) :& hts) = writeField @l r v >> writeFieldsWrk @hts r hts
    {-# INLINE writeFieldsWrk #-}

---------------------------------------------------------------------------------------------------
-- | Find index of given label in record.
type family LabelIndex (label :: Symbol) (ts :: [Type]) :: Nat where
    LabelIndex l ts = LabelIndexWrk l ts 0

type family LabelIndexWrk (label :: Symbol) (ts :: [Type]) (n :: Nat) :: Nat where
    LabelIndexWrk l '[]               n = TypeError ('Text "Label " ':<>: 'ShowType l ':<>: 'Text " not found.")
    LabelIndexWrk l ((l  := v) ': ts) n = n
    LabelIndexWrk l ((l' := v) ': ts) n = LabelIndexWrk l ts (n + 1)

-- | Type of field with given label.
type family LabelType (label :: Symbol) (ts :: [Type]) :: Type where
    LabelType l '[]               = TypeError ('Text "Label " ':<>: 'ShowType l ':<>: 'Text " not found.")
    LabelType l ((l  := v) ': ts) = v
    LabelType l ((l' := v) ': ts) = LabelType l ts

-- | Get offset and size of the field with given label.
type family LabelLayout (label :: Symbol) (ts :: [Type]) :: (Nat, Nat) where
    LabelLayout l ts = LabelLayoutWrk l (Layout ts)

type family LabelLayoutWrk (label :: Symbol) (layout :: [(Symbol, (Nat, Nat))]) :: (Nat, Nat) where
    LabelLayoutWrk l '[]                = TypeError ('Text "Label " ':<>: 'ShowType l ':<>: 'Text " not found.")
    LabelLayoutWrk l ('( l , v ) ': xs) = v
    LabelLayoutWrk l ('( l', v ) ': xs) = LabelLayoutWrk l xs 

---------------------------------------------------------------------------------------------------
-- | Read a field with given label from a pointer to record with given types.
readField :: forall (l :: Symbol) (ts :: [Type]) m.
          ( ReadFieldCtx l ts
          , PrimMonad m
          )
          => Rec ts
          -> m (LabelType l ts)
readField (Rec fp) = peekOff fp (natVal_ @(Fst (LabelLayout l ts)))
{-# INLINE readField #-}

-- | Constraints for 'readField'.
type ReadFieldCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal_ (Fst (LabelLayout l ts))
    )

-- | Write a field with given label to a pointer to record with given types
writeField :: forall (l :: Symbol) (ts :: [Type]) m.
          ( WriteFieldCtx l ts
          , PrimMonad m
          )
          => Rec ts
          -> LabelType l ts
          -> m ()
writeField (Rec fp) = pokeOff fp (natVal_ @(Fst (LabelLayout l ts)))
{-# INLINE writeField #-}

-- | Constraints for 'writeField'.
type WriteFieldCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal_ (Fst (LabelLayout l ts))
    )

---------------------------------------------------------------------------------------------------
-- | Read a value at given offset from given pointer.
peekOff :: forall a m.
        ( Storable a
        , PrimMonad m
        )
        => ForeignPtr ()
        -> Int
        -> m a
peekOff fptr offset =
    unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
        peekByteOff (castPtr @_ @a ptr) offset
{-# INLINE peekOff #-}

-- | Write a value to the memory at given offset from given pointer.
pokeOff :: forall a m.
        ( Storable a
        , PrimMonad m
        )
        => ForeignPtr ()
        -> Int
        -> a
        -> m ()
pokeOff fptr offset x = do
    unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
        pokeByteOff (castPtr @_ @a ptr) offset x
{-# INLINE pokeOff #-}

---------------------------------------------------------------------------------------------------
-- | Workaround for https://ghc.haskell.org/trac/ghc/ticket/14170
class NatVal_ (n :: Nat) where
    natVal_ :: Int

$( pure $ (flip map) [0..99] $ \i ->
        InstanceD Nothing [] (ConT ''NatVal_ `AppT` LitT (NumTyLit i)) [FunD 'natVal_ [Clause [] (NormalB $ LitE $ IntegerL i) []]]
 )
