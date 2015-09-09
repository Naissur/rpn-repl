module RPN where

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing


data UnaryOperation = Sin
                     |Cos
                     |Sqrt
                     |Log
                     |Exp deriving (Show, Eq)

data BinaryOperation = Add
                      |Sub 
                      |Mul 
                      |Div deriving (Show, Eq)

data Operation = Binary BinaryOperation
                |Unary UnaryOperation deriving (Show, Eq)

data Number = Num Double deriving (Eq)
instance Show Number where
    show (Num n) = show n

applyBinaryOperation :: BinaryOperation -> Number -> Number -> Either String Number
applyBinaryOperation Add (Num a) (Num b) = Right (Num (a + b))
applyBinaryOperation Sub (Num a) (Num b) = Right (Num (a - b))
applyBinaryOperation Mul (Num a) (Num b) = Right (Num (a * b))
applyBinaryOperation Div (Num a) (Num b) = 
                if b == 0 then 
                    Left "Computation error: Division by zero"
                else 
                    Right (Num (a / b))

applyUnaryOperation :: UnaryOperation -> Number -> Either String Number
applyUnaryOperation Sin  (Num a) = Right (Num (sin a))
applyUnaryOperation Cos  (Num a) = Right (Num (cos a))
applyUnaryOperation Sqrt (Num a) = Right (Num (sqrt a))
applyUnaryOperation Exp  (Num a) = Right (Num (exp a))
applyUnaryOperation Log  (Num a) = 
                    if (a < 0)  then 
                        Left ("Computation error: Cannot take logarithm of " ++ (show a) ) 
                    else 
                        Right (Num (log a))


data NumberStack = Stack [Number] deriving (Show)

emptyStack :: NumberStack
emptyStack = Stack []

popFromStack :: NumberStack -> Either String (NumberStack, Number)
popFromStack (Stack []) = Left "Computation error: cannot pop from empty stack"
popFromStack (Stack (x:xs)) = Right (Stack xs, x)

pushToStack :: Number -> NumberStack -> NumberStack
pushToStack num (Stack nums) = Stack (num:nums)


data Token = OperationToken Operation
            |NumberToken Number deriving (Show, Eq)


stepComputation :: Token -> NumberStack -> Either String NumberStack
stepComputation (NumberToken n) stack  = Right (pushToStack n stack )
stepComputation (OperationToken (Unary op)) stack = 
                                do
                                    (stack', arg) <- popFromStack stack
                                    result <- applyUnaryOperation op arg
                                    return $ pushToStack result stack'

stepComputation (OperationToken (Binary op)) stack = 
                                do
                                    (stack', firstArg) <- popFromStack stack
                                    (stack'', secondArg) <- popFromStack stack'
                                    result <- applyBinaryOperation op secondArg firstArg
                                    return $ pushToStack result stack'' 


data Expression = Expr [Token] deriving (Show)


evalExpression :: NumberStack -> Expression -> Either String Number
evalExpression stack (Expr [] ) = 
                    do
                        (_, popped) <- popFromStack stack
                        return $ popped
                    
evalExpression stack (Expr (token:restTokens) ) = 
                    do 
                        nextStack <- stepComputation token stack
                        evalExpression nextStack (Expr restTokens) 
