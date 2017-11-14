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
import Data.Coerce
import Data.List
import Data.Kind
import Data.Singletons.Prelude               hiding (type (+), type (-))
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Promoted
import GHC.OverloadedLabels
import GHC.Types
import GHC.TypeLits                          hiding (natVal)
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
    {-# INLINE fromLabel #-}

---------------------------------------------------------------------------------------------------
-- | Mutable anonymous record.
newtype MRec s (ts :: [Type]) = MRec { _mrecPtr :: (ForeignPtr ()) }

instance (ReadFields ts, Eq (HList ts)) => Eq (MRec s ts) where
    a == b = unsafeInlineIO $ do
        fa <- readFields @ts (coerce a)
        fb <- readFields @ts (coerce b)
        pure (fb == fa)

instance (ReadFields ts, Show (HList ts)) => Show (MRec s ts) where
    show r = "MRec " ++ show (unsafeInlineIO $ readFields @ts $ coerce r)

type instance SizeOf (MRec s ts) = RecSize ts

type instance Alignment (MRec s '[]      ) = 1
type instance Alignment (MRec s (t ': ts)) = Alignment t `Max` Alignment (MRec s ts)

instance ( NatVal (RecSize ts)
         , NatVal (Alignment (MRec s ts))
         ) => Storable (MRec s ts)
    where
    sizeOf _ = natVal @(SizeOf (MRec s ts))
    alignment _ = natVal @(Alignment (MRec s ts))

    peek src = do
        r@(MRec fp) <- newMRec @ts
        withForeignPtr fp $ \p ->
            copyBytes p (castPtr src) (natVal @(RecSize ts))
        pure $ coerce r

    poke dest (MRec fp) = do
        withForeignPtr fp $ \p ->
            copyBytes dest (castPtr p) (natVal @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Create a record from 'HList' of fields.
mrecord :: forall (ts :: [Type]) (m :: Type -> Type).
        ( NatVal (RecSize ts)
        , WriteFieldsWrk ts ts
        , PrimMonad m
        )
        => HList ts
        -> m (MRec (PrimState m) ts)
mrecord hl = do
    r <- newMRec @ts
    writeFields @ts r hl
    pure r
{-# INLINE mrecord #-}

-- | Make copy of the record.
clone :: forall (ts :: [Type]) m.
      ( NatVal (RecSize ts)
      , PrimMonad m
      )
      => MRec (PrimState m) ts
      -> m (MRec (PrimState m) ts)
clone (MRec fp0) = do
    r1@(MRec fp1) <- newMRec @ts
    unsafePrimToPrim $ withForeignPtr fp0 $ \p0 ->
        withForeignPtr fp1 $ \p1 ->
            copyBytes p1 p0 (natVal @(RecSize ts))
    pure r1
{-# INLINE clone #-}

---------------------------------------------------------------------------------------------------
-- | Heterogenous list.
data HList (ts :: [Type]) where
    (:&) :: t -> HList ts -> HList (t ': ts)
    Nil  :: HList '[]

infixr 5 :&

-------------------------------------------------
instance Eq (HList '[]) where
    Nil == Nil = True
    {-# INLINE (==) #-}

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :& as) == (b :& bs) = a == b && as == bs
    {-# INLINE (==) #-}

-------------------------------------------------
instance (ShowHList ts) => Show (HList ts) where
    show hl = "[ " ++ intercalate " , " (showHList hl) ++ " ]"

class ShowHList (ts :: [Type]) where
    showHList :: HList ts -> [String]

instance ShowHList '[] where
    showHList Nil = []
    {-# INLINE showHList #-}

instance (Show t, ShowHList ts) => ShowHList (t ': ts) where
    showHList (a :& as) = show a : showHList as
    {-# INLINE showHList #-}

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
--  layoutWrk sizeAcc = (offset, sizeOf t) : layoutWrk ts sizeAcc'
--      where
--          offset   = sizeAcc + padding
--          padding  = (alignment t - sizeAcc) `mod` alignment t
--          sizeAcc' = offset + sizeOf t

-- | Modulo operation.
type family Mod (a :: Nat) (b :: Nat) :: Nat where
    Mod a 0 = TypeError ('Text "Mod " ':<>: 'ShowType a ':<>: 'Text "0 - division by zero.")
    Mod a a = 0
    Mod 0 a = 0
    Mod a 1 = 0
    Mod a b = If (a <=? b) a (Mod (a - b) b)

-- | Absolute value of a difference between two nats.
type family Diff (a :: Nat) (b :: Nat) :: Nat where
    Diff a b = DiffWrk a b (a <=? b)

type family DiffWrk (a :: Nat) (b :: Nat) (a_lte_b :: Bool) :: Nat where
    DiffWrk a b 'True  = b - a
    DiffWrk a b 'False = a - b

---------------------------------------------------------------------------------------------------
-- | Allocate memory for the record. The memory is not initialized.
newMRec :: forall (ts :: [Type]) m.
        ( NatVal (RecSize ts)
        , PrimMonad m
        ) 
        => m (MRec (PrimState m) ts)
newMRec = unsafePrimToPrim $ MRec <$> mallocForeignPtrBytes (natVal @(RecSize ts))
{-# INLINE newMRec #-}

---------------------------------------------------------------------------------------------------
-- | Read fields from record.
readFields :: forall (ts :: [Type]) m.
           ( ReadFields ts
           , PrimMonad m
           )
           => MRec (PrimState m) ts
           -> m (HList ts)
readFields = readFieldsWrk @ts @ts
{-# INLINE readFields #-}

-- | Constraints for 'readFields'.
type ReadFields (ts :: [Type]) = ReadFieldsWrk ts ts

class ReadFieldsWrk (hts :: [Type]) (rts :: [Type]) where
    readFieldsWrk :: (PrimMonad m) => MRec (PrimState m) rts -> m (HList hts)

instance ReadFieldsWrk '[] rts where
    readFieldsWrk _ = pure Nil
    {-# INLINE readFieldsWrk #-}

instance ( ReadFieldsWrk hts rts
         , ReadFieldCtx l rts
         , KnownSymbol l
         , LabelType l rts ~ v
         ) => ReadFieldsWrk ((l := v) ': hts) rts where
    readFieldsWrk r = do
        v <- readField @l FldProxy r
        (FldProxy @l := v :&) <$> readFieldsWrk @hts r
    {-# INLINE readFieldsWrk #-}

-------------------------------------------------
-- | Write fields into record.
writeFields :: forall (ts :: [Type]) (m :: Type -> Type).
            ( WriteFields ts
            , PrimMonad m
            )
            => MRec (PrimState m) ts
            -> HList ts
            -> m ()
writeFields = writeFieldsWrk @ts @ts
{-# INLINE writeFields #-}

-- | Constraints for 'writeFields'.
type WriteFields (ts :: [Type]) = WriteFieldsWrk ts ts

class WriteFieldsWrk (hts :: [Type]) (rts :: [Type]) where
    writeFieldsWrk :: (PrimMonad m) => MRec (PrimState m) rts -> HList hts -> m ()

instance WriteFieldsWrk '[] rts where
    writeFieldsWrk _ _ = pure ()
    {-# INLINE writeFieldsWrk #-}

instance ( WriteFieldsWrk hts rts
         , WriteFieldCtx l rts
         , LabelType l rts ~ v
         ) => WriteFieldsWrk ((l := v) ': hts) rts
    where
    writeFieldsWrk r ((_l := v) :& hts) = writeField @l FldProxy r v >> writeFieldsWrk @hts r hts
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

-- | Offset of the field with the label in the record.
type LabelOffset (l :: Symbol) (ts :: [Type]) = Fst (LabelLayout l ts)

-- | Size of the field with the label.
type LabelSize (l :: Symbol) (ts :: [Type]) = Snd (LabelLayout l ts)

---------------------------------------------------------------------------------------------------
-- | Read a field with given label from a pointer to record with given types.
readField :: forall (l :: Symbol) (ts :: [Type]) m.
          ( ReadFieldCtx l ts
          , PrimMonad m
          )
          => FldProxy l
          -> MRec (PrimState m) ts
          -> m (LabelType l ts)
readField _ (MRec fp) = peekOff fp (natVal @(LabelOffset l ts))
{-# INLINE readField #-}

-- | Constraints for 'readField'.
type ReadFieldCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal (LabelOffset l ts)
    )

-- | Write a field with given label to a pointer to record with given types
writeField :: forall (l :: Symbol) (ts :: [Type]) m.
          ( WriteFieldCtx l ts
          , PrimMonad m
          )
          => FldProxy l
          -> MRec (PrimState m) ts
          -> LabelType l ts
          -> m ()
writeField _ (MRec fp) = pokeOff fp (natVal @(LabelOffset l ts))
{-# INLINE writeField #-}

-- | Constraints for 'writeField'.
type WriteFieldCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal (LabelOffset l ts)
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
class NatVal (n :: Nat) where
    natVal :: Int

$( pure $ (flip map) [0..99] $ \i ->
        InstanceD
            Nothing
            []
            (ConT ''NatVal `AppT` LitT (NumTyLit i))
            [ FunD 'natVal [Clause [] (NormalB $ LitE $ IntegerL i) []]
            , PragmaD $ InlineP 'natVal Inline FunLike AllPhases
            ]
 )
