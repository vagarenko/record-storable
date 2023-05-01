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
      describe "and read all fields" $ do
        specifyPrimM
          (do
            r <- mrecord hlist
            readFields r)
          (`shouldBe` hlist)

      describe "and read one field" $ do
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
        
      describe "and write all fields" $ do
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

      describe "and write one field" $ do
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

      describe "and clone the record" $ do
        describe "records should have different locations in memory" $ do
          specifyPrimM
            (do
              r <- mrecord hlist
              r1 <- clone r
              pure (_mrecPtr r, _mrecPtr r1))
            (\(ptr, ptr1) -> ptr `shouldNotBe` ptr1)
        describe "and modifying the clone shouldn't change original" $ do
          specifyPrimM
            (do
              r <- mrecord hlist
              r1 <- clone r
              writeField #a r1 'z'
              readFields r1)
            (`shouldNotBe` hlist)

    describe "allocate memory for the record" $ do
      describe "and write all fields" $ do
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

      describe "and write one field" $ do
        specifyPrimM
          (do
            r <- newMRec @Fields
            writeField #d r 9
            readField #d r)
          (`shouldBe` 9)

    it "can be used from C code" $ do
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

        