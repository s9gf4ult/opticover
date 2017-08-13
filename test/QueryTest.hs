module QueryTest where

import Control.Exception
import Opticover
import Test.Tasty
import Test.Tasty.HUnit

case_simpleLiftField :: Assertion
case_simpleLiftField = do
  let
    a = Portal (Point 0 0) "a"
    b = Portal (Point 1 1) "b"
    c = Portal (Point 1 0) "c"
    links = linksMap [link a c, link c b]
    now = link a b
    fields = liftFields now links
  field <- case fields of
    [a] -> return a
    x   -> error $ "Unexpected fields list " ++ show x
  fieldSquare field @?= 0.5

case_doubleLiftField :: Assertion
case_doubleLiftField = do
  let
    a = Portal (Point 0 0) "a"
    b = Portal (Point 1 1) "b"
    c = Portal (Point 1 0) "c"
    d = Portal (Point 0 1) "d"
    links = linksMap [link a c, link c b, link a d, link d b]
    now = link a b
    fields = liftFields now links
  (f1, f2) <- case fields of
    [f1, f2] -> return (f1, f2)
    x   -> error $ "Unexpected fields list " ++ show x
  fieldSquare f1 @?= 0.5
  fieldSquare f2 @?= 0.5

case_doubleMaxField :: Assertion
case_doubleMaxField = do
  let
    a = Portal (Point 0 0) "a"
    b = Portal (Point 1 1) "b"
    c = Portal (Point 1 0) "c"
    e = Portal (Point 0.75 0.25) "e"
    d = Portal (Point 0 1) "d"
    f = Portal (Point 0.25 0.75) "f"
    links = linksMap
      [ link a c, link c b, link a d, link d b
      , link a e, link e b, link a f, link f b ]
    now = link a b
    fields = liftFields now links
  (f1, f2) <- case fields of
    [f1, f2] -> return (f1, f2)
    x   -> error $ "Unexpected fields list " ++ show x
  fieldSquare f1 @?= 0.5
  fieldSquare f2 @?= 0.5
