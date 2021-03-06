module Control.Monad.Trans.Except.Exit (
    orDie
  , orDieWithCode
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           System.Exit (ExitCode(..), exitWith)
import           System.IO (stderr, hPutStrLn)

import           Control.Monad.Trans.Except (ExceptT, runExceptT)

-- | orDieWithCode with an exit code of 1 in case of an error
--
orDie :: (e -> Text) -> ExceptT e IO a -> IO a
orDie = orDieWithCode 1

-- | An idiom for failing hard on EitherT errors.
--
-- *This really dies*. There is no other way to say it.
--
-- The reason it lives with command line parser tooling, is that it is
-- the only valid place to actually exit like this. Be appropriately
-- wary.
--
orDieWithCode :: Int -> (e -> Text) -> ExceptT e IO a -> IO a
orDieWithCode code render e =
  runExceptT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitWith (ExitFailure code)) pure
