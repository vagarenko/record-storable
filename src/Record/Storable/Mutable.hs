{-# LANGUAGE 
      GADTs
    , TypeFamilies
    , PolyKinds
    , DataKinds
    , AllowAmbiguousTypes
    , UndecidableInstances
#-}

module Record.Storable.Mutable (
    -- * Record field
      (:=)(..)
    , FldProxy(..)
    -- * Mutable record
    , MRec(..)
    -- ** Heterogenous list
    , HList(..)
    , ShowHList
    -- ** Record creation
    , mrecord
    , clone
    , newMRec
    -- ** Record info
    , RecSize
    , RecAlignment
    , Layout
    -- ** Accessing elements
    , readFields
    , ReadFields
    , writeFields
    , WriteFields
    , readField
    , ReadField
    , writeField
    , WriteField
    -- ** Label info
    , LabelIndex
    , LabelType
    , LabelLayout
    , LabelOffset
    , LabelSize
    , demoteInt
) where

import Control.Monad.Primitive
import Data.Coerce
import Data.List
import Data.Kind
import Prelude.Singletons                    hiding (type (+), type (-))
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Promoted
import GHC.OverloadedLabels
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

instance PStorable (l := v) where
    type SizeOf    (l := v) = SizeOf v
    type Alignment (l := v) = Alignment v

---------------------------------------------------------------------------------------------------
-- | A 'Proxy' restricted to 'Symbol' kind.
data FldProxy (t :: Symbol) = FldProxy
    deriving (Show, Read, Eq, Ord)

instance (l ~ l') => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel = FldProxy
    {-# INLINE fromLabel #-}

---------------------------------------------------------------------------------------------------
-- | Mutable anonymous record.
newtype MRec s (ts :: [Type]) = MRec { _mrecPtr :: ForeignPtr () }

instance (ReadFields ts, Eq (HList ts)) => Eq (MRec s ts) where
    a == b = unsafeInlineIO $ do
        fa <- readFields @ts (coerce a)
        fb <- readFields @ts (coerce b)
        pure (fb == fa)
    {-# INLINE (==) #-}

instance (ReadFields ts, Show (HList ts)) => Show (MRec s ts) where
    show r = "MRec " ++ show (unsafeInlineIO $ readFields @ts $ coerce r)
    {-# INLINE show #-}

instance PStorable (MRec s ts) where
    type SizeOf    (MRec s ts) = RecSize ts
    type Alignment (MRec s ts) = RecAlignment ts

instance ( KnownNat (RecSize ts)
         , KnownNat (Alignment (MRec s ts))
         ) => Storable (MRec s ts)
    where
    {-# INLINE alignment #-}
    {-# INLINE sizeOf #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}

    sizeOf _ = demoteInt @(SizeOf (MRec s ts))
    alignment _ = demoteInt @(Alignment (MRec s ts))

    peek src = do
        r@(MRec fp) <- newMRec @ts
        withForeignPtr fp $ \p ->
            copyBytes p (castPtr src) (demoteInt @(RecSize ts))
        pure $ coerce r

    poke dest (MRec fp) = do
        withForeignPtr fp $ \p ->
            copyBytes dest (castPtr p) (demoteInt @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Create a record from 'HList' of fields.
mrecord :: forall (ts :: [Type]) (m :: Type -> Type).
        ( KnownNat (RecSize ts)
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
      ( KnownNat (RecSize ts)
      , PrimMonad m
      )
      => MRec (PrimState m) ts
      -> m (MRec (PrimState m) ts)
clone (MRec fp0) = do
    r1@(MRec fp1) <- newMRec @ts
    unsafePrimToPrim $ withForeignPtr fp0 $ \p0 ->
        withForeignPtr fp1 $ \p1 ->
            copyBytes p1 p0 (demoteInt @(RecSize ts))
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

-- | Helper class for 'Show' 'HList' instance.
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
    RecSize ts = CalcSize (Zip (RecFieldsSizes ts) (RecFieldsAlignments ts))

-- | Sizes of the record fields.
type family RecFieldsSizes (ts :: [Type]) :: [Nat] where
    RecFieldsSizes '[]       = '[]
    RecFieldsSizes ((l := t) ': rest) = SizeOf t ': RecFieldsSizes rest

-- | Alignments of the record.
type family RecAlignment (ts :: [Type]) :: Nat where
    RecAlignment ts = CalcAlignment (RecFieldsAlignments ts)

-- | Alignments of the record fields.
type family RecFieldsAlignments (ts :: [Type]) :: [Nat] where
    RecFieldsAlignments '[]       = '[]
    RecFieldsAlignments ((l := t) ': rest) = Alignment t ': RecFieldsAlignments rest

type family RecFieldLabels (ts :: [Type]) :: [Symbol] where
    RecFieldLabels '[]                = '[]
    RecFieldLabels ((l := t) ': rest) = l ': RecFieldLabels rest

-- | List of offsets and sizes of each label.
type family Layout (ts :: [Type]) :: [(Symbol, (Nat, Nat))] where
    Layout ts = Zip (RecFieldLabels ts) (Zip (CalcOffsets (Zip (RecFieldsSizes ts) (RecFieldsAlignments ts))) (RecFieldsSizes ts))

---------------------------------------------------------------------------------------------------
-- | Allocate memory for the record. The memory is not initialized.
newMRec :: forall (ts :: [Type]) m.
        ( KnownNat (RecSize ts)
        , PrimMonad m
        ) 
        => m (MRec (PrimState m) ts)
newMRec = unsafePrimToPrim $ MRec <$> mallocForeignPtrBytes (demoteInt @(RecSize ts))
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
         , ReadField l rts
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
         , WriteField l rts
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
          ( ReadField l ts
          , PrimMonad m
          )
          => FldProxy l
          -> MRec (PrimState m) ts
          -> m (LabelType l ts)
readField _ (MRec fp) = peekOff fp (demoteInt @(LabelOffset l ts))
{-# INLINE readField #-}

-- | Constraints for 'readField'.
type ReadField (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , KnownNat (LabelOffset l ts)
    )

-- | Write a field with given label to a pointer to record with given types
writeField :: forall (l :: Symbol) (ts :: [Type]) m.
          ( WriteField l ts
          , PrimMonad m
          )
          => FldProxy l
          -> MRec (PrimState m) ts
          -> LabelType l ts
          -> m ()
writeField _ (MRec fp) = pokeOff fp (demoteInt @(LabelOffset l ts))
{-# INLINE writeField #-}

-- | Constraints for 'writeField'.
type WriteField (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , KnownNat (LabelOffset l ts)
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

demoteInt :: forall (n :: Nat). KnownNat n => Int
demoteInt = fromInteger . natVal $ Proxy @n
