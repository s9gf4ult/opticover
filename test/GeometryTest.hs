module GeometryTest where

import Control.Exception
import Opticover
import Test.Tasty
import Test.Tasty.HUnit

case_parallelLinesNoIntersect :: Assertion
case_parallelLinesNoIntersect = do
  let
    l1 = Line $ unordPair (Point 0 0) (Point 1 0)
    l2 = Line $ unordPair (Point 0 1) (Point 1 1)
    cp = lineCrossPoint l1 l2
  cp @?= Nothing

case_perpLinesIntersect :: Assertion
case_perpLinesIntersect = do
  let
    l1 = Line $ unordPair (Point 1 0) (Point 0 0)
    l2 = Line $ unordPair (Point 0 0) (Point 0 1)
    cp = lineCrossPoint l1 l2
  cp @?= Just (Point 0 0)

case_angledLinesIntersect :: Assertion
case_angledLinesIntersect = do
  let
    l1 = Line $ unordPair (Point 1 1) (Point 2 2)
    l2 = Line $ unordPair (Point 2 1) (Point 3 1)
    cp = lineCrossPoint l1 l2
  cp @?= Just (Point 1 1)
