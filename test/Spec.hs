{-# LANGUAGE 
    OverloadedLabels
  , DataKinds
  , QuasiQuotes
  , TemplateHaskell
#-}

module Main where

import Record.Storable
import Record.Storable.Mutable
import Test.Hspec
import Data.Int
import Data.Char
import Control.Monad.ST
import Data.Functor.Identity (Identity(runIdentity))
import Control.Monad.Primitive
import Test.Hspec.Core.Spec
import Foreign.C.Types
import qualified Language.C.Inline as C

C.context (C.baseCtx <> C.fptrCtx)
C.include "stdint.h"
C.include "<stdio.h>"

type Fields =
  '[ "a" := Char, "b" := Bool, "c" := Int8, "d" := Double, "e" := Float]

hlist :: HList Fields
hlist = 
     (#a := 'a')
  :& (#b := False)
  :& (#c := (0 :: Int8))
  :& (#d := (1 :: Double))
  :& (#e := (2 :: Float))
  :& Nil

specifyPrimM :: (Show a, Eq a) => (forall m. PrimMonad m => m a) -> (a -> Expectation) -> SpecM () ()
specifyPrimM action mkExpectation = do
  it "in IO" $ do
    a <- action
    mkExpectation a
  it "in ST" $ mkExpectation (runST action)

main :: IO ()
main = hspec $ do
  describe "Mutable record" $ do
    describe "create record from HList" $ do
      describe "read all fields" $ do
        specifyPrimM
          (do
            r <- mrecord hlist
            readFields r)
          (`shouldBe` hlist)

      describe "read one field" $ do
        specifyPrimM
          (do
            r <- mrecord hlist
            (,,,,) 
              <$> readField #a r
              <*> readField #b r
              <*> readField #c r
              <*> readField #d r
              <*> readField #e r)
          (`shouldBe` ('a', False, 0, 1, 2))
        
      describe "write all fields" $ do
        let newHlist = 
                 (#a := 'b')
              :& (#b := True)
              :& (#c := (3 :: Int8))
              :& (#d := (4 :: Double))
              :& (#e := (5 :: Float))
              :& Nil
        specifyPrimM
          (do
            r <- mrecord hlist
            writeFields r newHlist
            readFields r)
          (`shouldBe` newHlist)

      describe "write one field" $ do
        specifyPrimM
          (do
            r <- mrecord hlist
            writeField #c r 1
            readFields r)
          (`shouldBe`
               (#a := 'a')
            :& (#b := False)
            :& (#c := (1 :: Int8))
            :& (#d := (1 :: Double))
            :& (#e := (2 :: Float))
            :& Nil)

      describe "clone the record" $ do
        describe "records should have different locations in memory" $ do
          specifyPrimM
            (do
              r <- mrecord hlist
              r1 <- clone r
              pure (_mrecPtr r, _mrecPtr r1))
            (\(ptr, ptr1) -> ptr `shouldNotBe` ptr1)
        describe "modifying the clone shouldn't change original" $ do
          specifyPrimM
            (do
              r <- mrecord hlist
              r1 <- clone r
              writeField #a r1 'z'
              readFields r1)
            (`shouldNotBe` hlist)

    describe "allocate memory for the record" $ do
      describe "write all fields" $ do
        let newHlist = 
                 (#a := 'c')
              :& (#b := True)
              :& (#c := (6 :: Int8))
              :& (#d := (7 :: Double))
              :& (#e := (8 :: Float))
              :& Nil
        specifyPrimM
          (do
            r <- newMRec @Fields
            writeFields r newHlist
            readFields r)
          (`shouldBe` newHlist)

      describe "write one field" $ do
        specifyPrimM
          (do
            r <- newMRec @Fields
            writeField #d r 9
            readField #d r)
          (`shouldBe` 9)

    it "can be read from C code" $ do
      r <- mrecord hlist
      ca <- [C.block|
        uint32_t {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          return r->a;
        }
      |]
      a <- readField #a r
      chr (fromIntegral ca) `shouldBe` a

      cb <- [C.block|
        uint32_t {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          return r->b;
        }
      |]
      b <- readField #b r
      b `shouldBe` if cb == 0 then False else True

      cc <- [C.block|
        int8_t {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          return r->c;
        }
      |]
      c <- readField #c r
      cc `shouldBe` c

      CDouble cd <- [C.block|
        double {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          return r->d;
        }
      |]
      d <- readField #d r
      cd `shouldBe` d

      CFloat ce <- [C.block|
        float {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          return r->e;
        }
      |]
      e <- readField #e r
      ce `shouldBe` e

    it "can be modified from C code" $ do
      r <- mrecord hlist
      [C.block|
        void {
          typedef struct {
            uint32_t a;
            uint32_t b;
            int8_t   c;
            double   d;
            float    e;
          } Rec;
          Rec *r = $fptr-ptr:(void *r);
          r->a = 'b';
          r->b = 1;
          r->c = 10;
          r->d = 11.5;
          r->e = 12.3;
        }
      |]
      a <- readField #a r
      b <- readField #b r
      c <- readField #c r
      d <- readField #d r
      e <- readField #e r
      a `shouldBe` 'b'
      b `shouldBe` True
      c `shouldBe` 10
      d `shouldBe` 11.5
      e `shouldBe` 12.3
  
  describe "Immutable record" $ do
    describe "create record from HList" $ do
      before (pure $ record hlist) $ do
        it "get all fields" $ \r -> do
          getFields r `shouldBe` hlist

        it "get one field" $ \r -> do
          getField #a r `shouldBe` 'a'
          getField #b r `shouldBe` False
          getField #c r `shouldBe` 0
          getField #d r `shouldBe` 1
          getField #e r `shouldBe` 2
        
        it "set field" $ \r -> do
          getField #a (setField #a r 'c'   ) `shouldBe` 'c' 
          getField #b (setField #b r True  ) `shouldBe` True
          getField #c (setField #c r 20    ) `shouldBe` 20
          getField #d (setField #d r 0.5   ) `shouldBe` 0.5
          getField #e (setField #e r (-0.9)) `shouldBe` (-0.9)

        it "modify field" $ \r -> do
          getField #a (modifyField #a r succ) `shouldBe` 'b' 
          getField #b (modifyField #b r not ) `shouldBe` True
          getField #c (modifyField #c r (+1)) `shouldBe` 1
          getField #d (modifyField #d r (+2)) `shouldBe` 3
          getField #e (modifyField #e r (+3)) `shouldBe` 5

    describe "freeze mutable record" $ do
      specifyPrimM
        (do
          mr <- mrecord hlist
          r  <- freeze mr
          pure $ getFields r
        )
        (`shouldBe` hlist)
      
    describe "thaw immutable record" $ do
      specifyPrimM
        (do
          let r = record hlist
          mr <- thaw r
          readFields mr
        )
        (`shouldBe` hlist)

    describe "unsafe freeze mutable record" $ do
      specifyPrimM
        (do
          mr <- mrecord hlist
          r  <- unsafeFreeze mr
          pure $ getFields r
        )
        (`shouldBe` hlist)
      
    describe "unsafe thaw immutable record" $ do
      specifyPrimM
        (do
          let r = record hlist
          mr <- unsafeThaw r
          readFields mr
        )
        (`shouldBe` hlist)
      
    describe "copy immutable record into a mutable one" $ do
      specifyPrimM
        (do
          let r = record hlist
          mr <- newMRec @Fields
          copy r mr
          readFields mr
        )
        (`shouldBe` hlist)
