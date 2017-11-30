module SpecUtils (module X, module SpecUtils) where

import           Test.Hspec as X hiding (expectationFailure)

rightWith :: (Show a, Show e, Eq a) => Either e a -> a -> IO ()
rightWith eea expected = case eea of
  Right a -> a `shouldBe` expected
  (Left left)    -> expectationFailure $ "Not Right: Left " ++ show left

shouldBeLeft :: (Show a) => Either e a -> IO ()
shouldBeLeft eea = case eea of
  Left _ -> pure ()
  Right right  -> expectationFailure $ "Not Left: Right " ++ show right

asRight :: Show e => Either e a -> (a -> IO b) -> IO b
asRight eea f = case eea of
  Right a -> f a
  Left left -> expectationFailure $ "Not Right: Left " ++ show left

expectationFailure :: String -> IO a
expectationFailure = error

shouldBeIO :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldBeIO ioa expected = do
  a <- ioa
  a `shouldBe` expected
