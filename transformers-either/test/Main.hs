{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (ArithException (..), IOException, SomeException, bracket, finally, fromException, throwIO, try)
import           Control.Monad (unless)
import           Control.Monad.Catch (Handler (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Functor.Identity (Identity (..))
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either.Exit (orDieWithCode)

import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Text as T

import           GHC.IO.Handle (hDuplicate, hDuplicateTo)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Exit (ExitCode (..), exitFailure)
import           System.IO (BufferMode (..), hClose, hSetBuffering, openTempFile, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ok <- checkSequential tests
  unless ok exitFailure

tests :: Group
tests = Group "Control.Monad.Trans.Either"
  [ ("prop_constructors", prop_constructors)
  , ("prop_handleLeftT", prop_handleLeftT)
  , ("prop_handlesEitherT", prop_handlesEitherT)
  , ("prop_bracketEitherT", prop_bracketEitherT)
  , ("prop_bracketExceptionT", prop_bracketExceptionT)
  , ("prop_bracketExceptionT_base_short_circuit", prop_bracketExceptionT_base_short_circuit)
  , ("prop_mapping", prop_mapping)
  , ("prop_handleEitherT", prop_handleEitherT)
  , ("prop_hushM_onLeft_onNothing", prop_hushM_onLeft_onNothing)
  , ("prop_orDieWithCode", prop_orDieWithCode)
  , ("prop_bracketExceptionT_async", prop_bracketExceptionT_async)
  ]

------------------------------------------------------------------------
-- Generators

genErr :: Gen String
genErr = Gen.string (Range.linear 0 10) Gen.alpha

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-100) 100)

genEither :: Gen (Either String Int)
genEither = Gen.either genErr genInt

-- | What an action under test does.
data Outcome
  = Return Int
  | ThrowArith
  | ThrowIO
  deriving (Eq, Show)

genOutcome :: Gen Outcome
genOutcome = Gen.choice [Return <$> genInt, pure ThrowArith, pure ThrowIO]

runOutcome :: Outcome -> IO Int
runOutcome o = case o of
  Return n -> return n
  ThrowArith -> throwIO DivideByZero
  ThrowIO -> throwIO (userError "io")

-- | Handlers that can be installed, in a known order.
data HandlerSpec
  = HArith
  | HSome
  deriving (Eq, Show)

toHandler :: HandlerSpec -> Handler IO String
toHandler h = case h of
  HArith -> Handler (\(_ :: ArithException) -> return "arith")
  HSome -> Handler (\(_ :: SomeException) -> return "some")

matches :: HandlerSpec -> Outcome -> Bool
matches h o = case (h, o) of
  (_, Return _) -> False
  (HArith, ThrowArith) -> True
  (HArith, _) -> False
  (HSome, _) -> True

-- | A release action that counts how often it runs, then produces the given
-- result.
countingRelease :: MonadIO m => IORef Int -> Either String () -> a -> EitherT String m ()
countingRelease ref result _ = do
  liftIO (modifyIORef ref (+ 1))
  hoistEither result

-- | Expected result of a bracket whose acquire succeeded: an error from
-- release takes precedence over the body's result.
expectedBracket :: Either String () -> Either String Int -> Either String Int
expectedBracket rel body = either Left (const body) rel

-- | What the body of a bracket does.
data BodyOutcome
  = BodyEither (Either String Int)
  | BodyThrow
  deriving (Eq, Show)

genBodyOutcome :: Gen BodyOutcome
genBodyOutcome = Gen.choice [BodyEither <$> genEither, pure BodyThrow]

runBody :: BodyOutcome -> EitherT String IO Int
runBody b = case b of
  BodyEither ea -> hoistEither ea
  BodyThrow -> liftIO (throwIO DivideByZero)

-- | Exit codes, biased towards 0 and the boundaries where the low 8 bits wrap.
genExitCode :: Gen Int
genExitCode = Gen.frequency
  [ (3, Gen.int (Range.linearFrom 1 (-1000) 1000))
  , (1, Gen.element [-256, -1, 0, 1, 255, 256, 257, 512])
  ]

-- | Run an action with stderr redirected to a temporary file.
silenceStderr :: IO a -> IO a
silenceStderr action = do
  tmp <- getTemporaryDirectory
  bracket
    (do (path, h) <- openTempFile tmp "stderr.txt"
        old <- hDuplicate stderr
        hDuplicateTo h stderr
        return (path, h, old))
    (\(path, h, old) -> do
        hDuplicateTo old stderr
        hClose old
        hClose h
        removeFile path)
    (const action)

------------------------------------------------------------------------
-- Properties

prop_constructors :: Property
prop_constructors = property $ do
  e <- forAll genErr
  a <- forAll genInt
  ea <- forAll genEither
  ma <- forAll (Gen.maybe genInt)
  r1 <- evalIO $ runEitherT (left e :: EitherT String IO Int)
  r2 <- evalIO $ runEitherT (right a :: EitherT String IO Int)
  r3 <- evalIO $ runEitherT (hoistEither ea :: EitherT String IO Int)
  r4 <- evalIO $ runEitherT (hoistMaybe e ma :: EitherT String IO Int)
  r1 === Left e
  r2 === Right a
  r3 === ea
  r4 === maybe (Left e) Right ma

-- | handleLeftT only invokes the handler on Left, and catchLeftT is its flip.
prop_handleLeftT :: Property
prop_handleLeftT = property $ do
  ea <- forAll genEither
  hr <- forAll genEither
  let handler e = hoistEither (fmap (+ length e) hr) :: EitherT String IO Int
      expected = either (\e -> fmap (+ length e) hr) Right ea
  r1 <- evalIO $ runEitherT (handleLeftT handler (hoistEither ea))
  r2 <- evalIO $ runEitherT (catchLeftT (hoistEither ea) handler)
  r1 === expected
  r2 === expected

-- | handlesEitherT picks the first matching handler, and rethrows any exception
-- no handler matches.
prop_handlesEitherT :: Property
prop_handlesEitherT = property $ do
  outcome <- forAll genOutcome
  specs <- forAll (Gen.list (Range.linear 0 3) (Gen.element [HArith, HSome]))
  r <- evalIO . try $ runEitherT (handlesEitherT (map toHandler specs) (runOutcome outcome))
  let expected = case outcome of
        Return n -> Just (Right n)
        _ -> case filter (`matches` outcome) specs of
          (HArith : _) -> Just (Left "arith")
          (HSome : _) -> Just (Left "some")
          [] -> Nothing
  case r of
    Right v -> Just v === expected
    Left (_ :: SomeException) -> Nothing === expected

-- | bracketEitherT releases exactly once iff acquire succeeded, whatever the
-- body returns.
prop_bracketEitherT :: Property
prop_bracketEitherT = property $ do
  acq <- forAll (Gen.either genErr (pure ()))
  body <- forAll genEither
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  r <- evalIO $ runEitherT
    (bracketEitherT (hoistEither acq) (countingRelease ref rel) (\_ -> hoistEither body))
  n <- evalIO (readIORef ref)
  case acq of
    Left e -> do
      r === Left e
      n === 0
    Right () -> do
      r === expectedBracket rel body
      n === 1

-- | bracketExceptionT releases exactly once iff acquire succeeded, including
-- when the body throws, and rethrows exceptions from the body (as its Haddock
-- documents).
prop_bracketExceptionT :: Property
prop_bracketExceptionT = property $ do
  acq <- forAll (Gen.either genErr (pure ()))
  body <- forAll genBodyOutcome
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  r <- evalIO . try $ runEitherT
    (bracketExceptionT (hoistEither acq) (countingRelease ref rel) (\_ -> runBody body))
  n <- evalIO (readIORef ref)
  case acq of
    Left e -> do
      r === Right (Left e)
      n === 0
    Right () -> do
      n === 1
      case body of
        BodyThrow -> r === Left DivideByZero
        BodyEither ea -> r === Right (expectedBracket rel ea)

-- | bracketExceptionT documents that it cleans up "regardless of 'left' or
-- exception". That must include the base monad short-circuiting (here
-- MaybeT returning Nothing).
prop_bracketExceptionT_base_short_circuit :: Property
prop_bracketExceptionT_base_short_circuit = property $ do
  body <- forAll (Gen.maybe genEither)
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  let run = maybe (lift (MaybeT (return Nothing))) hoistEither body
             :: EitherT String (MaybeT IO) Int
  r <- evalIO . runMaybeT $ runEitherT
    (bracketExceptionT (right ()) (countingRelease ref rel) (\_ -> run))
  n <- evalIO (readIORef ref)
  n === 1
  r === fmap (expectedBracket rel) body

-- | The mapping combinators agree with their pure 'Either' counterparts.
prop_mapping :: Property
prop_mapping = property $ do
  ea <- forAll genEither
  let m = hoistEither ea :: EitherT String Identity Int
      f = reverse
      g = (* 2)
      run = runIdentity . runEitherT
      bimapE f' g' = either (Left . f') (Right . g')
  run (bimapEitherT f g m) === bimapE f g ea
  run (firstEitherT f m) === bimapE f id ea
  run (secondEitherT g m) === bimapE id g ea
  run (mapEitherT (fmap (bimapE f g)) m) === bimapE f g ea
  runIdentity (eitherT (return . length) (return . g) m) === either length g ea
  r <- evalIO $ runEitherT (hoistEitherT (return . runIdentity) m)
  r === ea

-- | handleIOEitherT and handleEitherT catch exactly the requested exception
-- type and let everything else escape. The catch variants are their flips.
prop_handleEitherT :: Property
prop_handleEitherT = property $ do
  outcome <- forAll genOutcome
  let action = runOutcome outcome
      expectIO = case outcome of
        Return n -> Just (Right n)
        ThrowIO -> Just (Left "io")
        ThrowArith -> Nothing
      expectArith = case outcome of
        Return n -> Just (Right n)
        ThrowArith -> Just (Left "arith")
        ThrowIO -> Nothing
      wrapIO = const "io" :: IOException -> String
      wrapArith = const "arith" :: ArithException -> String
      toMaybe :: Either SomeException a -> Maybe a
      toMaybe = either (const Nothing) Just
  r1 <- evalIO . try $ runEitherT (handleIOEitherT wrapIO action :: EitherT String IO Int)
  r2 <- evalIO . try $ runEitherT (catchIOEitherT action wrapIO :: EitherT String IO Int)
  r3 <- evalIO . try $ runEitherT (handleEitherT wrapArith action)
  r4 <- evalIO . try $ runEitherT (catchEitherT action wrapArith)
  toMaybe r1 === expectIO
  toMaybe r2 === expectIO
  toMaybe r3 === expectArith
  toMaybe r4 === expectArith

-- | hushM, onLeft and onNothing run their handler exactly when the value is
-- Left / Nothing, and pass Right / Just values through.
prop_hushM_onLeft_onNothing :: Property
prop_hushM_onLeft_onNothing = property $ do
  ea <- forAll genEither
  ma <- forAll (Gen.maybe genInt)
  ref <- evalIO (newIORef (0 :: Int))
  let bump = modifyIORef ref (+ 1)
  r1 <- evalIO $ hushM ea (const bump)
  r2 <- evalIO $ onLeft (\e -> bump >> return (length e)) (return ea)
  r3 <- evalIO $ onNothing (bump >> return 0) (return ma)
  n <- evalIO (readIORef ref)
  r1 === either (const Nothing) Just ea
  r2 === either length id ea
  r3 === maybe 0 id ma
  -- hushM and onLeft each bump once on Left; onNothing bumps once on Nothing.
  n === 2 * either (const 1) (const 0) ea + maybe 1 (const 0) ma

-- | orDieWithCode returns the value on Right. On Left it terminates with
-- ExitFailure code. (ExitFailure 0 is invalid, so code 0 makes exitWith throw
-- an IOError instead, which still terminates the process with a failure.
-- GHC's top-level handler also maps codes whose low 8 bits are 0 to 255, so
-- the process never reports success.)
prop_orDieWithCode :: Property
prop_orDieWithCode = property $ do
  code <- forAll genExitCode
  ea <- forAll genEither
  r <- evalIO . silenceStderr . try $
    orDieWithCode code T.pack (hoistEither ea)
  case (ea, r) of
    (Right a, Right b) ->
      b === a
    (Left _, Left (ex :: SomeException)) -> do
      annotateShow ex
      case fromException ex of
        Just (ExitFailure c) ->
          c === code
        _ ->
          code === 0
    _ -> do
      annotateShow (fmap (const ()) r)
      failure

-- | Killing the thread while the body runs still releases exactly once, and
-- release can be asked to fail without affecting that.
prop_bracketExceptionT_async :: Property
prop_bracketExceptionT_async = withTests 20 . property $ do
  rel <- forAll (Gen.either genErr (pure ()))
  ref <- evalIO (newIORef (0 :: Int))
  n <- evalIO $ do
    inBody <- newEmptyMVar
    done <- newEmptyMVar
    let body _ = liftIO (putMVar inBody () >> threadDelay 10000000) :: EitherT String IO ()
    tid <- forkIO $
      (() <$ runEitherT (bracketExceptionT (right ()) (countingRelease ref rel) body))
        `finally` putMVar done ()
    takeMVar inBody
    killThread tid
    takeMVar done
    readIORef ref
  n === 1
