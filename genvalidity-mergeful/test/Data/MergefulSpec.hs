{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergefulSpec
  ( spec
  ) where

import Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.GenValidity.UUID.Typed ()

import Data.Mergeful
import Data.Mergeful.Item

import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeful.Item

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  genValidSpec @(ClientStore (UUID Int) Int)
  genValidSpec @(ServerStore (UUID Int) Int)
  genValidSpec @(SyncRequest (UUID Int) Int)
  genValidSpec @(SyncResponse (UUID Int) Int)
  describe "makeSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeSyncResponseIgnoreProblems" $
    it "produces valid requests" $ producesValidsOnValids2 (mergeSyncResponseIgnoreProblems @Int @Int)
  describe "processServerSync" $
    it "produces valid tuples of a response and a store" $
    producesValidsOnValids2
      (\store request ->
         evalD $ processServerSync genD (store :: ServerStore (UUID Int) Int) request)

newtype D a =
  D
    { unD :: State StdGen a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen)

evalD :: D a -> a
evalD d = fst $ runD d $ mkStdGen 42

runD :: D a -> StdGen -> (a, StdGen)
runD = runState . unD

genD :: D (Typed.UUID a)
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u
