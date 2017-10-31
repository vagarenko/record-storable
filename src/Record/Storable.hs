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

import Data.Kind
import Data.Proxy
import Data.Singletons.Prelude               hiding (type (+), type (-))
import Data.Singletons.TypeLits              (Mod)
import Debug.Trace
import Foreign.Storable
import Foreign.Storable.Promoted
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.OverloadedLabels
import GHC.Types
import GHC.TypeLits
import Language.Haskell.TH                   hiding (Type)
import qualified Language.Haskell.TH as TH

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

-- instance (ShowRec ts) => Show (Rec ts) where
--     show = show . showRec

-- class ShowRec (ts :: [Type]) where
--     showRec :: Rec ts -> [String]

-- instance ShowRec '[] where
--     showRec _ = []

-- instance ShowRec ((l := v) ': ts) where
--     showRec r = 

type instance SizeOf (Rec ts) = RecSize ts

type instance Alignment (Rec '[]      ) = 1
type instance Alignment (Rec (t ': ts)) = Alignment t `Max` Alignment (Rec ts)

---------------------------------------------------------------------------------------------------
-- | Create a record from 'HList' of fields.
record :: forall (ts :: [Type]).
       ( NatVal_ (RecSize ts)
       , WriteFields ts ts
       )
       => HList ts
       -> IO (Rec ts)
record hl = do
    r <- mallocRec @ts
    writeFieldsWrk @ts @ts r hl
    pure r

rrr :: IO (Rec '["a" := Int, "b" := Float, "c" := Double])
rrr = record
    $  #a := (0 :: Int)
    :& #b := (0.1 :: Float)
    :& #c := (0.00000000001 :: Double)
    :& Nil

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
mallocRec :: forall (ts :: [Type]). (NatVal_ (RecSize ts)) => IO (Rec ts)
mallocRec = Rec <$> mallocForeignPtrBytes (natVal_ @(RecSize ts))

---------------------------------------------------------------------------------------------------
-- | Read fields from record.
readFields :: forall (ts :: [Type]). (ReadFields ts ts) => Rec ts -> IO (HList ts)
readFields = readFieldsWrk @ts @ts

class ReadFields (hts :: [Type]) (rts :: [Type]) where
    readFieldsWrk :: Rec rts -> IO (HList hts)

instance ReadFields '[] rts where
    readFieldsWrk _ = pure Nil
    {-# INLINE readFieldsWrk #-}

instance ( ReadFields hts rts
         , ReadLabelCtx l rts
         , KnownSymbol l
         , LabelType l rts ~ v
         ) => ReadFields ((l := v) ': hts) rts where
    readFieldsWrk r = do
        v <- readLabel @l r
        (FldProxy @l := v :&) <$> readFieldsWrk @hts r
    {-# INLINE readFieldsWrk #-}

-------------------------------------------------
-- | Write fields into record.
writeFields :: forall (ts :: [Type]). (WriteFields ts ts) => Rec ts -> HList ts -> IO ()
writeFields = writeFieldsWrk @ts @ts

class WriteFields (hts :: [Type]) (rts :: [Type]) where
    writeFieldsWrk :: Rec rts -> HList hts -> IO ()

instance WriteFields '[] rts where
    writeFieldsWrk _ _ = pure ()
    {-# INLINE writeFieldsWrk #-}

instance ( WriteFields hts rts
         , WriteLabelCtx l rts
         , LabelType l rts ~ v
         ) => WriteFields ((l := v) ': hts) rts
    where
    writeFieldsWrk r ((_l := v) :& hts) = writeLabel @l r v >> writeFieldsWrk @hts r hts
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
readLabel :: forall (l :: Symbol) (ts :: [Type]).
          (ReadLabelCtx l ts)
          => Rec ts
          -> IO (LabelType l ts)
readLabel (Rec fp) = peekOff fp (natVal_ @(Fst (LabelLayout l ts)))
{-# INLINE readLabel #-}

-- | Constraints for 'readLabel'.
type ReadLabelCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal_ (Fst (LabelLayout l ts))
    )

-- | Write a field with given label to a pointer to record with given types
writeLabel :: forall (l :: Symbol) (ts :: [Type]).
          (WriteLabelCtx l ts)
          => Rec ts
          -> LabelType l ts
          -> IO ()
writeLabel (Rec fp) = pokeOff fp (natVal_ @(Fst (LabelLayout l ts)))
{-# INLINE writeLabel #-}

-- | Constraints for 'writeLabel'.
type WriteLabelCtx (l :: Symbol) (ts :: [Type]) =
    ( Storable (LabelType l ts)
    , NatVal_ (Fst (LabelLayout l ts))
    )

---------------------------------------------------------------------------------------------------
-- | Read a value at given offset from given pointer.
peekOff :: forall a.
        (Storable a)
        => ForeignPtr ()
        -> Int
        -> IO a
peekOff fptr offset =
    withForeignPtr fptr $ \ptr ->
        peekByteOff (castPtr @_ @a ptr) offset
{-# INLINE peekOff #-}

-- | Write a value to the memory at given offset from given pointer.
pokeOff :: forall a. 
        (Storable a)
        => ForeignPtr ()
        -> Int
        -> a
        -> IO ()
pokeOff fptr offset x = do
    withForeignPtr fptr $ \ptr ->
        pokeByteOff (castPtr @_ @a ptr) offset x
{-# INLINE pokeOff #-}


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


-- | Family of variadic functions. Argument types are encoded in type-level list.
-- > '[a, b, c, ...] ~~> r   ~   a -> b -> c -> ... -> r
-- type family (~~>) (argTypes :: [Type]) (result :: TYPE rep) :: Type where
--     '[]        ~~> r = r
--     (t ':  ts) ~~> r = t -> (ts ~~> r)

-- infixr 0 ~~>

-- recc :: forall (ts :: [Type]). ts ~~> ((##) -> Rec ts)
-- recc = undefined
        

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
-- | Workaround for https://ghc.haskell.org/trac/ghc/ticket/14170
class NatVal_ (n :: Nat) where
    natVal_ :: Int

$( pure $ (flip map) [0..99] $ \i ->
        InstanceD Nothing [] (ConT ''NatVal_ `AppT` LitT (NumTyLit i)) [FunD 'natVal_ [Clause [] (NormalB $ LitE $ IntegerL i) []]]
 )
