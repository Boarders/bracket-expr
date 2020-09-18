{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Data.Ratio
import Control.Monad
import System.IO.Unsafe
import Debug.Trace
import Data.Char (isDigit)
import Text.Earley
import Data.Tree.Pretty
import Data.Tree
import Data.Foldable (traverse_)
import Diagrams.TwoD.Layout.Tree
import Diagrams
import Diagrams.Backend.SVG
import Control.Lens hiding ((#), none)
import Data.Colour.Names
import Prelude hiding (tan)
import System.Directory




main :: IO ()
main = do
  let input = "5 * 8 - 6 / 2"
  putStrLn $ "input : " <> input
  let exprTrees = parseExpr input
  traverse_ printExprTreeAndVal exprTrees
  let width  = 150
  let height = 350
  let fileName n = "expr" <> show n <> ".svg"
  let size       = mkSizeSpec (V2 (Just width) (Just height))
  let createImage (t,n) =
        let
         valS = printValue . eval $ t
       in
         renderSVG (fileName n) size (diagTree valS . exprToTreeS $ t)
  let createImages = traverse_ createImage (zip exprTrees [1..])
  withCurrentDirectory "images" $ do
    getCurrentDirectory >>= print
    createImages
    
data Expr =
    Lit Int
  | Op Op Expr Expr
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)


opToStr :: Op -> String
opToStr = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "÷"


strToOp :: String -> Either String Op
strToOp "+" = pure Add
strToOp "-" = pure Sub
strToOp "*" = pure Mul
strToOp "/" = pure Div
sstrToOp str =
  Left $ "Expected one of " <> show ops <> "but received: " <> str
  




interpOp :: (Fractional a) => Op -> a -> a -> a
interpOp = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> (/)
  


eval :: Expr -> Maybe (Ratio Int)
eval = \case
  Lit n -> Just (fromIntegral n)
  Op Div expr1 expr2 ->
    let
      val2 = eval expr2
    in
      case val2 of
        Just 0 -> Nothing
        _      -> liftA2 (/) (eval expr1) val2
  Op op expr1 expr2  -> liftA2 (interpOp op) (eval expr1) (eval expr2)

printValue :: Maybe (Ratio Int) -> String
printValue = \case
  Nothing -> "Division by zero in expression"
  Just rat -> case (numerator rat, denominator rat) of
    (n, 1) -> show n
    (n, m) -> show n <> " / " <> show m
  

ops :: [String]
ops = ["+", "-", "*", "/"]

isNum :: String -> Bool
isNum = all isDigit

data Token = TOp Op | TLit Int

printTok :: Token -> String
printTok = \case
  TLit n -> show n
  TOp op -> opToStr op

exprToTree :: Expr -> Tree Token
exprToTree = \case
  Lit n -> Node (TLit n) []
  Op op e1 e2 -> Node (TOp op) [exprToTree e1, exprToTree e2]

exprToTreeS :: Expr -> Tree String
exprToTreeS = fmap printTok . exprToTree


printExprTreeAndVal :: Expr -> IO ()
printExprTreeAndVal e = do
  putStrLn ""
  putStrLn "Expression: "
  putStrLn $ unlines . indent 15 . lines . drawVerticalTree . fmap printTok . exprToTree $ e
  putStrLn ""
  let value = eval e
  putStrLn $ "value: " <> printValue value
  putStrLn ""  
  where
    indent n = fmap ((replicate n ' ') <>)



exprParser :: Grammar r (Prod r String String Expr)
exprParser = mdo
  x1 <- rule $ Op Add <$> x1 <* namedToken "+" <*> x1
           <|> Op Sub <$> x1 <* namedToken "-" <*> x1
           <|> Op Mul <$> x1 <* namedToken "*" <*> x1
           <|> Op Div <$> x1 <* namedToken "/" <*> x1           
           <|> Lit . read <$> (satisfy isNum <?> "identifier")
           <|> namedToken "(" *> x1 <* namedToken ")"
  return x1             
    

parseExpr :: String -> [Expr]
parseExpr str = fst . fullParses (parser exprParser) $ words str


diagTree :: String -> Tree String  -> Diagram B
diagTree val t = vcat $
  [ strutY 1
  , renderTree ((<> circle 1 # fc orange # lw none) . text # fc purple)
             (~~)
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t)
      # centerXY # pad 1.1
  , strutY 1
  , text ("value : " <> val <> "") # fc purple      
  ]
