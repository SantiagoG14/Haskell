-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = do
    x <- px
    return (f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\x -> [(a, x)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = do
    f <- pg
    x <- px
    return (f x)

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P g) >>= f =
    P
      ( \inp -> case g inp of
          [] -> []
          [(x, s)] -> let (P z) = f x in z s
      )

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

three :: Parser (Char, Char)
-- three = g <$> item <*> item <*> item
--   where
--     g x y z = (x, z)

three = do
  x <- item
  y <- item
  z <- item
  return (x, z)