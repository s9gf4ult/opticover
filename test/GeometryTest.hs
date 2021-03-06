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

case_pointInTriangle :: Assertion
case_pointInTriangle = do
  let
    a = Point 0 0
    b = Point 1 0
    c = Point 0 1
    t = Triangle $ unordTriple a b c
  -- on points
  True @=? pointInTriangle t a
  True @=? pointInTriangle t b
  True @=? pointInTriangle t c
  -- on edges
  True @=? pointInTriangle t (Point 0.5 0)
  True @=? pointInTriangle t (Point 0 0.5)
  True @=? pointInTriangle t (Point 0.5 0.5)
  -- inside trialge
  True @=? pointInTriangle t (Point 0.25 0.25)

case_pointNotInTriangle :: Assertion
case_pointNotInTriangle = do
  let
    a = Point 0 0
    b = Point 1 0
    c = Point 0 1
    t = Triangle $ unordTriple a b c
  False @=? pointInTriangle t (Point 1 1)
  False @=? pointInTriangle t (Point 2 0)
  False @=? pointInTriangle t (Point 0 2)
  False @=? pointInTriangle t (Point 0.5 (-0.5))
  False @=? pointInTriangle t (Point (-0.5) 0.5)
