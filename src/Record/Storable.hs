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
#-}

module Record.Storable where

import Control.Monad
import Data.Coerce
import Data.Kind
import Debug.Trace
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
-- import Control.Lens
-- import Data.Kind
-- import Data.List
-- import Data.Singletons
-- import Data.Singletons.Prelude.List
-- import Data.Singletons.TypeRepStar
-- import Data.Typeable
-- import Data.Type.Equality
-- import Foreign.ForeignPtr
-- import Foreign.Ptr
-- import Foreign.Storable
-- import Type.List
-- import GHC.IO ( IO(..) )
-- import System.IO.Unsafe (unsafePerformIO)
-- import qualified Data.Vector.Unboxed as VU


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


-- | A 'Proxy' restricted to 'Symbol' kind.
data FldProxy (t :: Symbol) = FldProxy
    deriving (Show, Read, Eq, Ord)

instance (l ~ l') => IsLabel (l :: Symbol) (FldProxy l') where
    fromLabel = FldProxy

-- |
data Rec (ts :: [Type]) = Rec
    { _recSize       :: !Int
    , _recLayout     :: !(VU.Vector ElemLayout)
    , _recPtr        :: !(ForeignPtr ())
    } deriving (Show, Eq)

data ElemLayout = ElemLayout 
    { _elemLayoutOffset  :: !Int
    , _elemLayoutSize    :: !Int
    } deriving (Eq, Ord, Show)


-- | Compute layout of elements of a 'Rec'.
layout :: forall (ts :: [Type]). (Layout ts) => VU.Vector ElemLayout
layout = VU.fromList $ layoutWrk @ts 0

-- | Worker for 'layout'.
class Layout (ts :: [Type]) where
    layoutWrk :: Int -> [ElemLayout]

instance Layout '[] where
    layoutWrk _ = []

instance (Layout ts, Storable t) => Layout (t ': ts) where
    layoutWrk sizeAcc = ElemLayout offset size : layoutWrk @ts sizeAcc'
        where
            offset   = sizeAcc + padding
            size     = sizeOf @t undefined
            align    = alignment @t undefined
            padding  = (align - sizeAcc) `mod` align
            sizeAcc' = offset + size

-- | Allocate memory for the record. The record will contain garbage.
mallocRec :: forall (ts :: [Type]). (Layout ts) => IO (Rec ts)
mallocRec = Rec size lt <$> mallocForeignPtrBytes size
    where
        lt   = layout @ts
        size = layoutSize lt

-- | Total size of given layout.
layoutSize :: VU.Vector ElemLayout -> Int
layoutSize v =
    let l = VU.last v
    in _elemLayoutOffset l + _elemLayoutSize l


-- | Write an element to the memory at given offset from given pointer.
pokeElem :: forall a. 
    (Storable a)
    => ForeignPtr ()
    -> Int
    -> a
    -> IO ()
pokeElem fptr offset x = do
    traceM $ "poke " ++ show offset
    withForeignPtr fptr $ \ptr ->
        pokeByteOff (castPtr @_ @a ptr) offset x

-- | Read an element at given offset from given pointer.
peekElem :: forall a.
    (Storable a)
    => ForeignPtr ()
    -> Int
    -> IO a
peekElem fptr offset =
    withForeignPtr fptr $ \ptr ->
        peekByteOff (castPtr @_ @a ptr) offset



-- ---------------------------------------------------------------------------------------------------
-- data HVector (ts :: [Type]) = HVector 
--     { hvectorSings      :: Sing ts
--     , hvectorSize       :: !Int
--     , hvectorLayout     :: ![ElemLayout]
--     , hvectorPtr        :: !(ForeignPtr ())
--     }

-- data ElemLayout = ElemLayout 
--     { elemLayoutOffset  :: !Int
--     , elemLayoutSize    :: !Int
--     } deriving (Eq, Ord, Show)

-- -- | Build a 'HVector'.
-- hvector :: forall (ts :: [Type]).
--     ( SingI ts
--     , Layout ts
--     , PokeElems ts
--     , ComposeV (IO (HVector ts)) (HVector ts) ts
--     , BindV IO (HVector ts) (HVector ts) ts
--     , Kleisli IO () (HVector ts) ts
--     ) 
--     => ts ~~> HVector ts
-- hvector =
--     composeV @(IO (HVector ts)) @(HVector ts) @ts
--         unsafePerformIO 
--         (bindV @IO @(HVector ts) @(HVector ts) @ts
--             (mallocHVector @ts)
--             (\hv@(HVector _ _ lay fptr) ->
--                 kleisli @IO @() @(HVector ts) @ts
--                     (pokeElems @ts fptr lay)
--                     (\_ -> pure hv)))

-- withHVector :: forall (ts :: [Type]) a. HVector ts -> (ts ~~> a) -> a
-- withHVector = undefined

-- -- | Write elements to the memory at given pointer.
-- class PokeElems (ts :: [Type]) where
--     pokeElems :: ForeignPtr () -> [ElemLayout] -> ts ~~> IO ()

-- instance PokeElems '[] where
--     pokeElems _ _ = pure ()

-- instance (Storable t, PokeElems ts, ThenV IO () () ts) => PokeElems (t ': ts) where
--     pokeElems fptr (ElemLayout offset _ : ls) a =
--         thenV @IO @() @() @ts (pokeElem @t fptr offset a) (pokeElems @ts fptr ls)
--     pokeElems _    []                         _ =
--         error "pokeElems: Impossible happend. Please report this bug."


-- -- | Family of variadic functions. Argument types are encoded in type-level list.
-- -- > '[a, b, c, ...] ~~> r   ~   a -> b -> c -> ... -> r
-- type family (~~>) (argTypes :: [Type]) (result :: Type) :: Type where
--     '[]        ~~> r = r
--     (t ':  ts) ~~> r = t -> (ts ~~> r)

-- infixr 0 ~~>

-- -- | Composition of ordinary function and variadic function.
-- class ComposeV b c (ts :: [Type]) where
--     composeV :: (b -> c) -> (ts ~~> b) -> ts ~~> c

-- instance ComposeV b c '[] where
--     composeV f b = f b

-- instance (ComposeV b c ts) => ComposeV b c (t ': ts) where
--     composeV :: (((t ': ts) ~~> b) ~ (t -> ts ~~> b)) => (b -> c) -> ((t ': ts) ~~> b) -> (t ': ts) ~~> c
--     composeV f g x = composeV @b @c @ts f (g x)

-- -- | Left-to-right Kleisli composition of variadic function and ordinary function.
-- --   Similar to '>=>'.
-- class Kleisli (m :: Type -> Type) a b (ts :: [Type]) where
--     kleisli :: (ts ~~> m a) -> (a -> m b) -> ts ~~> m b

-- instance (Monad m) => Kleisli m a b '[] where
--     kleisli ma g = ma >>= g

-- instance (Kleisli m a b ts) => Kleisli m a b (t ': ts) where
--     kleisli f g x = kleisli @m @a @b @ts (f x) g

-- -- | Perform first action `m a` then pass its result to a function `(a -> ts ~~> mb)`
-- --   which returns variadic function and return that function.
-- class BindV (m :: Type -> Type) a b (ts :: [Type]) where
--     bindV :: m a -> (a -> ts ~~> m b) -> ts ~~> m b

-- instance (Monad m) => BindV m a b '[] where
--     bindV ma f = ma >>= f

-- instance (BindV m a b ts) => BindV m a b (t ': ts) where
--     bindV ma f x = bindV @m @a @b @ts ma ((flip f) x)

-- -- | Monadic composition that discards result of the first action.
-- thenV :: forall (m :: Type -> Type) a b (ts :: [Type]).
--     (ThenV m a b ts) => 
--     m a -> (ts ~~> m b) -> ts ~~> m b
-- thenV ma f = bindV @m @a @b @ts ma (\_ -> f)

-- -- | Constraints of 'thenV' function.
-- type ThenV (m :: Type -> Type) a b (ts :: [Type]) = BindV m a b ts

---------------------------------------------------------------------------------------------------
-- Unboxed Vector instances for ElmeLayout
---------------------------------------------------------------------------------------------------
newtype instance VU.MVector s (ElemLayout) = MV_ElemLayout (VU.MVector s (Int, Int))
newtype instance VU.Vector    (ElemLayout) = V_ElemLayout  (VU.Vector    (Int, Int))

instance VGM.MVector VU.MVector ElemLayout where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}

    basicLength (MV_ElemLayout v) = VGM.basicLength v
    
    basicUnsafeSlice i n (MV_ElemLayout v) = MV_ElemLayout $ VGM.basicUnsafeSlice i n v
    
    basicOverlaps (MV_ElemLayout v1) (MV_ElemLayout v2) = VGM.basicOverlaps v1 v2
    
    basicUnsafeNew n = MV_ElemLayout <$> VGM.basicUnsafeNew n

    basicInitialize (MV_ElemLayout v) = VGM.basicInitialize v
    
    basicUnsafeReplicate n (ElemLayout o s) = MV_ElemLayout <$> VGM.basicUnsafeReplicate n (o, s)
    
    basicUnsafeRead (MV_ElemLayout v) i = uncurry ElemLayout <$> VGM.basicUnsafeRead v i
    
    basicUnsafeWrite (MV_ElemLayout v) i (ElemLayout o s) = VGM.basicUnsafeWrite v i (o, s)
    
    basicClear (MV_ElemLayout v) = VGM.basicClear v
    
    basicSet (MV_ElemLayout v) (ElemLayout o s) = VGM.basicSet v (o, s)
    
    basicUnsafeCopy (MV_ElemLayout v1) (MV_ElemLayout v2) = VGM.basicUnsafeCopy v1 v2
    
    basicUnsafeMove (MV_ElemLayout v1) (MV_ElemLayout v2) = VGM.basicUnsafeMove v1 v2
    
    basicUnsafeGrow (MV_ElemLayout v) n = MV_ElemLayout <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector ElemLayout where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}

    basicUnsafeFreeze (MV_ElemLayout v) = V_ElemLayout <$> VG.basicUnsafeFreeze v

    basicUnsafeThaw (V_ElemLayout v) = MV_ElemLayout <$> VG.basicUnsafeThaw v

    basicLength (V_ElemLayout v) = VG.basicLength v

    basicUnsafeSlice i n (V_ElemLayout v) = V_ElemLayout $ VG.basicUnsafeSlice i n v

    basicUnsafeIndexM (V_ElemLayout v) i = uncurry ElemLayout <$> VG.basicUnsafeIndexM v i

    basicUnsafeCopy (MV_ElemLayout mv) (V_ElemLayout v) = VG.basicUnsafeCopy mv v

    elemseq _ (ElemLayout o s) z =
        VG.elemseq (undefined :: VU.Vector a) o $ VG.elemseq (undefined :: VU.Vector a) s z

instance VU.Unbox ElemLayout