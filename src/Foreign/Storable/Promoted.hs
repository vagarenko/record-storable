{-# LANGUAGE 
      CPP
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , TypeFamilies
    , TypeInType
    , AllowAmbiguousTypes
    , TypeApplications 
    , TemplateHaskell
#-}

module Foreign.Storable.Promoted where

import Data.Kind
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Promoted.TH
import GHC.Fingerprint.Type
import GHC.TypeLits

-- | Type-level 'sizeOf'.
type family SizeOf (t :: Type) :: Nat
type instance SizeOf Bool        = $(pSizeOf @Bool       )
type instance SizeOf Char        = $(pSizeOf @Char       )
type instance SizeOf Double      = $(pSizeOf @Double     )
type instance SizeOf Float       = $(pSizeOf @Float      )
type instance SizeOf Int         = $(pSizeOf @Int        )
type instance SizeOf Int8        = $(pSizeOf @Int8       )
type instance SizeOf Int16       = $(pSizeOf @Int16      )
type instance SizeOf Int32       = $(pSizeOf @Int32      )
type instance SizeOf Int64       = $(pSizeOf @Int64      )
type instance SizeOf Word        = $(pSizeOf @Word       )
type instance SizeOf Word8       = $(pSizeOf @Word8      )
type instance SizeOf Word16      = $(pSizeOf @Word16     )
type instance SizeOf Word32      = $(pSizeOf @Word32     )
type instance SizeOf Word64      = $(pSizeOf @Word64     )
type instance SizeOf ()          = $(pSizeOf @()         )
type instance SizeOf Fingerprint = $(pSizeOf @Fingerprint)
type instance SizeOf IntPtr      = $(pSizeOf @IntPtr     )
type instance SizeOf WordPtr     = $(pSizeOf @WordPtr    )
type instance SizeOf CUIntMax    = $(pSizeOf @CUIntMax   )
type instance SizeOf CIntMax     = $(pSizeOf @CIntMax    )
type instance SizeOf CUIntPtr    = $(pSizeOf @CUIntPtr   )
type instance SizeOf CIntPtr     = $(pSizeOf @CIntPtr    )
type instance SizeOf CSUSeconds  = $(pSizeOf @CSUSeconds )
type instance SizeOf CUSeconds   = $(pSizeOf @CUSeconds  )
type instance SizeOf CTime       = $(pSizeOf @CTime      )
type instance SizeOf CClock      = $(pSizeOf @CClock     )
type instance SizeOf CSigAtomic  = $(pSizeOf @CSigAtomic )
type instance SizeOf CWchar      = $(pSizeOf @CWchar     )
type instance SizeOf CSize       = $(pSizeOf @CSize      )
type instance SizeOf CPtrdiff    = $(pSizeOf @CPtrdiff   )
type instance SizeOf CDouble     = $(pSizeOf @CDouble    )
type instance SizeOf CFloat      = $(pSizeOf @CFloat     )
type instance SizeOf CULLong     = $(pSizeOf @CULLong    )
type instance SizeOf CLLong      = $(pSizeOf @CLLong     )
type instance SizeOf CULong      = $(pSizeOf @CULong     )
type instance SizeOf CLong       = $(pSizeOf @CLong      )
type instance SizeOf CUInt       = $(pSizeOf @CUInt      )
type instance SizeOf CInt        = $(pSizeOf @CInt       )
type instance SizeOf CUShort     = $(pSizeOf @CUShort    )
type instance SizeOf CShort      = $(pSizeOf @CShort     )
type instance SizeOf CUChar      = $(pSizeOf @CUChar     )
type instance SizeOf CSChar      = $(pSizeOf @CSChar     )
type instance SizeOf CChar       = $(pSizeOf @CChar      )
#if MIN_VERSION_base(4,10,0)
type instance SizeOf CBool       = $(pSizeOf @CBool      )
#endif

-- | Type-level 'alignment'.
type family Alignment (t :: Type) :: Nat
type instance Alignment Bool        = $(pAlignment @Bool       )
type instance Alignment Char        = $(pAlignment @Char       )
type instance Alignment Double      = $(pAlignment @Double     )
type instance Alignment Float       = $(pAlignment @Float      )
type instance Alignment Int         = $(pAlignment @Int        )
type instance Alignment Int8        = $(pAlignment @Int8       )
type instance Alignment Int16       = $(pAlignment @Int16      )
type instance Alignment Int32       = $(pAlignment @Int32      )
type instance Alignment Int64       = $(pAlignment @Int64      )
type instance Alignment Word        = $(pAlignment @Word       )
type instance Alignment Word8       = $(pAlignment @Word8      )
type instance Alignment Word16      = $(pAlignment @Word16     )
type instance Alignment Word32      = $(pAlignment @Word32     )
type instance Alignment Word64      = $(pAlignment @Word64     )
type instance Alignment ()          = $(pAlignment @()         )
type instance Alignment Fingerprint = $(pAlignment @Fingerprint)
type instance Alignment IntPtr      = $(pAlignment @IntPtr     )
type instance Alignment WordPtr     = $(pAlignment @WordPtr    )
type instance Alignment CUIntMax    = $(pAlignment @CUIntMax   )
type instance Alignment CIntMax     = $(pAlignment @CIntMax    )
type instance Alignment CUIntPtr    = $(pAlignment @CUIntPtr   )
type instance Alignment CIntPtr     = $(pAlignment @CIntPtr    )
type instance Alignment CSUSeconds  = $(pAlignment @CSUSeconds )
type instance Alignment CUSeconds   = $(pAlignment @CUSeconds  )
type instance Alignment CTime       = $(pAlignment @CTime      )
type instance Alignment CClock      = $(pAlignment @CClock     )
type instance Alignment CSigAtomic  = $(pAlignment @CSigAtomic )
type instance Alignment CWchar      = $(pAlignment @CWchar     )
type instance Alignment CSize       = $(pAlignment @CSize      )
type instance Alignment CPtrdiff    = $(pAlignment @CPtrdiff   )
type instance Alignment CDouble     = $(pAlignment @CDouble    )
type instance Alignment CFloat      = $(pAlignment @CFloat     )
type instance Alignment CULLong     = $(pAlignment @CULLong    )
type instance Alignment CLLong      = $(pAlignment @CLLong     )
type instance Alignment CULong      = $(pAlignment @CULong     )
type instance Alignment CLong       = $(pAlignment @CLong      )
type instance Alignment CUInt       = $(pAlignment @CUInt      )
type instance Alignment CInt        = $(pAlignment @CInt       )
type instance Alignment CUShort     = $(pAlignment @CUShort    )
type instance Alignment CShort      = $(pAlignment @CShort     )
type instance Alignment CUChar      = $(pAlignment @CUChar     )
type instance Alignment CSChar      = $(pAlignment @CSChar     )
type instance Alignment CChar       = $(pAlignment @CChar      )
#if MIN_VERSION_base(4,10,0)
type instance Alignment CBool       = $(pAlignment @CBool      )
#endif
