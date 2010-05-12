module Main where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

-- AST
data PrimitiveDataType = String | Integer | Boolean
                       deriving Show
  
data Context = Context { ctName :: String 
                       , ctType :: PrimitiveDataType
                       , ctBehavioralContracts :: [BehavioralContract] }
             deriving Show

data BehavioralContract = Pull { ctActivation :: String
                               , ctDataRequirement :: String
                               , ctEmission :: Emission
                               }
                        | Push { ctActivation :: String
                               , ctDataRequirement :: String
                               , ctEmission :: Emission
                               }
                          deriving Show

data Emission = EmitEmpty | EmitSelf | EmitSelfOpt
              deriving Show
  
-- Parser
lexer = P.makeTokenParser haskellDef

whiteSpace= P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier= P.identifier lexer
reserved = P.reserved lexer
reservedOp= P.reservedOp lexer

dataTypeRef :: Parser PrimitiveDataType
dataTypeRef = do { reserved "Boolean"
                 ; return Boolean
                 }
              <|>
              do { reserved "Integer"
                 ; return Integer
                 }
              <?> "data type"

emission :: Parser Emission
emission = do { reserved "empty"
              ; return EmitEmpty
              }
           <|>
           do { reserved "self"
              ;  do { try (char '?')
                    ; return EmitSelfOpt
                    }
                 <|>
                 do { return EmitSelf }
              }

behavioralContract :: Parser BehavioralContract
behavioralContract = 
  do { char '<'
     ; do { reserved "pull"
          ; reserved "self"
          ; semi
          ; dataRequirement <- identifier
          ; semi
          ; emission <- emission
          ; char '>'
          ; return (Pull "self" dataRequirement emission)
          }
       <|>
       do { reserved "push"
          ; activation <- identifier
          ; semi
          ; dataRequirement <- identifier
          ; semi
          ; emission <- emission
          ; char '>'
          ; return (Push activation dataRequirement emission)
          }
     }
       
  
context :: Parser Context
context = do { reserved "context"
             ; name <- identifier
             ; reserved "as"
             ; ctype <- dataTypeRef
             ; char '{'
             ; behavioralContract <- sepEndBy behavioralContract semi
             ; char '}'
             ; return (Context name ctype behavioralContract)
             }
          
runLex :: Show a => Parser a -> String -> IO ()
runLex p input
  = parseTest (do{ whiteSpace
           ; x <- p
           ; eof
           ; return x
           }) input