module Parser where

import RPN



parseToken :: String -> Either String Token
parseToken "+" =    Right (OperationToken (Binary Add))
parseToken "-" =    Right (OperationToken (Binary Sub))
parseToken "*" =    Right (OperationToken (Binary Mul))
parseToken "/" =    Right (OperationToken (Binary Div))
parseToken "sin" =  Right (OperationToken (Unary Sin))
parseToken "cos" =  Right (OperationToken (Unary Cos))
parseToken "exp" =  Right (OperationToken (Unary Exp))
parseToken "ln" =   Right (OperationToken (Unary Log))
parseToken "sqrt" = Right (OperationToken (Unary Sqrt))
parseToken "pi" =   Right (NumberToken (Num pi))
parseToken "e" =    Right (NumberToken (Num (exp 1)))
parseToken "phi" =  Right (NumberToken (Num ( ( (sqrt 5) + 1) / 2 )))
parseToken s = maybe (Left ("Parsing error: Invalid token: "++s))
                     (Right . NumberToken . Num )
                     (readMaybe s :: Maybe Double) 


buildExpression :: String -> Either String Expression
buildExpression s = 
            fmap Expr . mapM parseToken . words $ s

            

evalRPNExpression :: String -> Either String Number
evalRPNExpression s = 
            do
                expr <- buildExpression s
                evalExpression emptyStack expr
            

