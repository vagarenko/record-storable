{-# LANGUAGE 
      CPP
    , TypeFamilies
    , PolyKinds
    , DataKinds
    , TemplateHaskell
    , UndecidableInstances
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Foreign.Storable.Promoted (
      PStorable(..)
    , GPStorable(..)
    , Diff
) where

import Data.Kind
import Data.Int
import Prelude.Singletons       (Max, Zip, If, Last, type (++), Fst)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Promoted.TH
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.TypeLits

-- | Promoted 'Storable'.
class PStorable (a :: Type) where
    -- | Type-level 'sizeOf'.
    type SizeOf a :: Nat
    type instance SizeOf a = CalcSize (Zip (GSizes (Rep a)) (GAlignments (Rep a)))

    -- | Type-level 'alignment'.
    type Alignment a :: Nat
    type instance Alignment a = CalcAlignment (GAlignments (Rep a))

class GPStorable (f :: Type -> Type) where
    type GSizes      f :: [Nat]
    type GAlignments f :: [Nat]

instance (PStorable c) => GPStorable (K1 i c) where
    type GSizes      (K1 i c) = '[SizeOf c]
    type GAlignments (K1 i c) = '[Alignment c]

instance (GPStorable f) => GPStorable (M1 i t f) where
    type GSizes      (M1 i t f) = GSizes f
    type GAlignments (M1 i t f) = GAlignments f

instance (GPStorable f, GPStorable g) => GPStorable (f :*: g) where
    type GSizes      (f :*: g) = GSizes f ++ GSizes g
    type GAlignments (f :*: g) = GAlignments f ++ GAlignments g

---------------------------------------------------------------------------------------------------
-- | Calculate the size of a product type from list of sizes and alignments of type's fields.
type family CalcSize (sizes_aligns :: [(Nat, Nat)]) :: Nat where
    CalcSize sas = Last (CalcOffsets sas) + Fst (Last sas)

-- | Calculate offsets of a product type from list of sizes and alignments of type's fields.
type family CalcOffsets (sizes_aligns :: [(Nat, Nat)]) :: [Nat] where
    CalcOffsets sas = CalcOffsetsWrk sas 0

type family CalcOffsetsWrk (sizes_aligns :: [(Nat, Nat)]) (size_acc :: Nat) :: [Nat] where
    CalcOffsetsWrk '[]              _   = '[]
    CalcOffsetsWrk ('(s, a) ': sas) acc = CalcOffsetsWrkWrk (acc + Padding a acc) s sas

type family CalcOffsetsWrkWrk (offset :: Nat) (size :: Nat) (sizes_aligns :: [(Nat, Nat)]) :: [Nat] where
    CalcOffsetsWrkWrk o s sas = o ': CalcOffsetsWrk sas (o + s)

type family Padding (align :: Nat) (size_acc :: Nat) :: Nat where
    Padding a acc = Mod (Diff a acc) a

-- | Calculate the alignment of a product type from list of alignments of type's fields.
type family CalcAlignment (aligns :: [Nat]) :: Nat where
    CalcAlignment '[]       = 1
    CalcAlignment (x ': xs) = x `Max` CalcAlignment xs

-- | Absolute value of a difference between two nats.
type family Diff (a :: Nat) (b :: Nat) :: Nat where
    Diff a b = DiffWrk a b (a <=? b)

type family DiffWrk (a :: Nat) (b :: Nat) (a_lte_b :: Bool) :: Nat where
    DiffWrk a b 'True  = b - a
    DiffWrk a b 'False = a - b

---------------------------------------------------------------------------------------------------
instance PStorable Bool        where type SizeOf Bool        = $(pSizeOf @Bool       ); type Alignment Bool        = $(pAlignment @Bool       )
instance PStorable Char        where type SizeOf Char        = $(pSizeOf @Char       ); type Alignment Char        = $(pAlignment @Char       )
instance PStorable Double      where type SizeOf Double      = $(pSizeOf @Double     ); type Alignment Double      = $(pAlignment @Double     )
instance PStorable Float       where type SizeOf Float       = $(pSizeOf @Float      ); type Alignment Float       = $(pAlignment @Float      )
instance PStorable Int         where type SizeOf Int         = $(pSizeOf @Int        ); type Alignment Int         = $(pAlignment @Int        )
instance PStorable Int8        where type SizeOf Int8        = $(pSizeOf @Int8       ); type Alignment Int8        = $(pAlignment @Int8       )
instance PStorable Int16       where type SizeOf Int16       = $(pSizeOf @Int16      ); type Alignment Int16       = $(pAlignment @Int16      )
instance PStorable Int32       where type SizeOf Int32       = $(pSizeOf @Int32      ); type Alignment Int32       = $(pAlignment @Int32      )
instance PStorable Int64       where type SizeOf Int64       = $(pSizeOf @Int64      ); type Alignment Int64       = $(pAlignment @Int64      )
instance PStorable Word        where type SizeOf Word        = $(pSizeOf @Word       ); type Alignment Word        = $(pAlignment @Word       )
instance PStorable Word8       where type SizeOf Word8       = $(pSizeOf @Word8      ); type Alignment Word8       = $(pAlignment @Word8      )
instance PStorable Word16      where type SizeOf Word16      = $(pSizeOf @Word16     ); type Alignment Word16      = $(pAlignment @Word16     )
instance PStorable Word32      where type SizeOf Word32      = $(pSizeOf @Word32     ); type Alignment Word32      = $(pAlignment @Word32     )
instance PStorable Word64      where type SizeOf Word64      = $(pSizeOf @Word64     ); type Alignment Word64      = $(pAlignment @Word64     )
instance PStorable ()          where type SizeOf ()          = $(pSizeOf @()         ); type Alignment ()          = $(pAlignment @()         )
instance PStorable Fingerprint where type SizeOf Fingerprint = $(pSizeOf @Fingerprint); type Alignment Fingerprint = $(pAlignment @Fingerprint)
instance PStorable IntPtr      where type SizeOf IntPtr      = $(pSizeOf @IntPtr     ); type Alignment IntPtr      = $(pAlignment @IntPtr     )
instance PStorable WordPtr     where type SizeOf WordPtr     = $(pSizeOf @WordPtr    ); type Alignment WordPtr     = $(pAlignment @WordPtr    )
instance PStorable CUIntMax    where type SizeOf CUIntMax    = $(pSizeOf @CUIntMax   ); type Alignment CUIntMax    = $(pAlignment @CUIntMax   )
instance PStorable CIntMax     where type SizeOf CIntMax     = $(pSizeOf @CIntMax    ); type Alignment CIntMax     = $(pAlignment @CIntMax    )
instance PStorable CUIntPtr    where type SizeOf CUIntPtr    = $(pSizeOf @CUIntPtr   ); type Alignment CUIntPtr    = $(pAlignment @CUIntPtr   )
instance PStorable CIntPtr     where type SizeOf CIntPtr     = $(pSizeOf @CIntPtr    ); type Alignment CIntPtr     = $(pAlignment @CIntPtr    )
instance PStorable CSUSeconds  where type SizeOf CSUSeconds  = $(pSizeOf @CSUSeconds ); type Alignment CSUSeconds  = $(pAlignment @CSUSeconds )
instance PStorable CUSeconds   where type SizeOf CUSeconds   = $(pSizeOf @CUSeconds  ); type Alignment CUSeconds   = $(pAlignment @CUSeconds  )
instance PStorable CTime       where type SizeOf CTime       = $(pSizeOf @CTime      ); type Alignment CTime       = $(pAlignment @CTime      )
instance PStorable CClock      where type SizeOf CClock      = $(pSizeOf @CClock     ); type Alignment CClock      = $(pAlignment @CClock     )
instance PStorable CSigAtomic  where type SizeOf CSigAtomic  = $(pSizeOf @CSigAtomic ); type Alignment CSigAtomic  = $(pAlignment @CSigAtomic )
instance PStorable CWchar      where type SizeOf CWchar      = $(pSizeOf @CWchar     ); type Alignment CWchar      = $(pAlignment @CWchar     )
instance PStorable CSize       where type SizeOf CSize       = $(pSizeOf @CSize      ); type Alignment CSize       = $(pAlignment @CSize      )
instance PStorable CPtrdiff    where type SizeOf CPtrdiff    = $(pSizeOf @CPtrdiff   ); type Alignment CPtrdiff    = $(pAlignment @CPtrdiff   )
instance PStorable CDouble     where type SizeOf CDouble     = $(pSizeOf @CDouble    ); type Alignment CDouble     = $(pAlignment @CDouble    )
instance PStorable CFloat      where type SizeOf CFloat      = $(pSizeOf @CFloat     ); type Alignment CFloat      = $(pAlignment @CFloat     )
instance PStorable CULLong     where type SizeOf CULLong     = $(pSizeOf @CULLong    ); type Alignment CULLong     = $(pAlignment @CULLong    )
instance PStorable CLLong      where type SizeOf CLLong      = $(pSizeOf @CLLong     ); type Alignment CLLong      = $(pAlignment @CLLong     )
instance PStorable CULong      where type SizeOf CULong      = $(pSizeOf @CULong     ); type Alignment CULong      = $(pAlignment @CULong     )
instance PStorable CLong       where type SizeOf CLong       = $(pSizeOf @CLong      ); type Alignment CLong       = $(pAlignment @CLong      )
instance PStorable CUInt       where type SizeOf CUInt       = $(pSizeOf @CUInt      ); type Alignment CUInt       = $(pAlignment @CUInt      )
instance PStorable CInt        where type SizeOf CInt        = $(pSizeOf @CInt       ); type Alignment CInt        = $(pAlignment @CInt       )
instance PStorable CUShort     where type SizeOf CUShort     = $(pSizeOf @CUShort    ); type Alignment CUShort     = $(pAlignment @CUShort    )
instance PStorable CShort      where type SizeOf CShort      = $(pSizeOf @CShort     ); type Alignment CShort      = $(pAlignment @CShort     )
instance PStorable CUChar      where type SizeOf CUChar      = $(pSizeOf @CUChar     ); type Alignment CUChar      = $(pAlignment @CUChar     )
instance PStorable CSChar      where type SizeOf CSChar      = $(pSizeOf @CSChar     ); type Alignment CSChar      = $(pAlignment @CSChar     )
instance PStorable CChar       where type SizeOf CChar       = $(pSizeOf @CChar      ); type Alignment CChar       = $(pAlignment @CChar      )
#if MIN_VERSION_base(4,10,0)
instance PStorable CBool       where type SizeOf CBool       = $(pSizeOf @CBool      ); type Alignment CBool       = $(pAlignment @CBool      )
#endif
