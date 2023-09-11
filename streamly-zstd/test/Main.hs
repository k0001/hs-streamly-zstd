{-# LANGUAGE OverloadedStrings #-}
module Main where

import Streamly.Data.Stream qualified as S
import Streamly.Zstd qualified as Z

--------------------------------------------------------------------------------

expAct :: (Eq a, Show a) => a -> a -> IO ()
expAct e a | e == a    = pure ()
           | otherwise = fail $ "Expected " <> show e <> ", got " <> show a


main :: IO ()
main = do
  expAct (Just "(\181/\253\NULH)\NUL\NULhello")
         (mconcat <$> S.toList (Z.compress 1 (S.fromList ["hello"])))

  expAct (Just "(\181/\253\NULH)\NUL\NULhello")
         (mconcat <$> S.toList (Z.compress 1 (S.fromList ["hel", "lo"])))

  expAct (Just "(\181/\253\NULX)\NUL\NULhello")
         (mconcat <$> S.toList (Z.compress 3 (S.fromList ["hello"])))

  expAct (Just "(\181/\253\NULX)\NUL\NULhello")
         (mconcat <$> S.toList (Z.compress 3 (S.fromList ["hel", "lo"])))

  expAct (Just "hello")
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULH)\NUL\NULhello"])))

  expAct (Just "hello")
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULH)", "\NUL\NULhello"])))

  expAct (Just "hello")
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULX)\NUL\NULhello"])))

  expAct (Just "hello")
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULX)", "\NUL\NULhello"])))

  expAct Nothing
         (mconcat <$> S.toList (Z.decompress (S.fromList ["whatever"])))

  -- See issue #1
  expAct Nothing
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULX)\NUL\NULhello4444"])))

  expAct Nothing
         (mconcat <$> S.toList (Z.decompress (S.fromList ["(\181/\253\NULX)\NUL\NULhello55555"])))
