import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

example1 = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

example2 = evalStr "(2+3)*4"
example3 = evalStr "2+3*4"
example4 = evalStr "2+3*"

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x   = Lit x
  add a b = Add a b
  mul a b = Mul a b

example5 = mul (add (lit 2) (lit 3)) (lit 4)
  == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
