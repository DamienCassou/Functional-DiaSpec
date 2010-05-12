module Main where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

-- AST
data Document = Document [Context]
              deriving Show

data DataType = String | Integer | Boolean | UserDT String
                deriving Show
  
data Context = Context { ctName :: String 
                       , ctType :: DataType
                       , ctBehavioralContracts :: [BehavioralContract] }
             deriving Show

data BehavioralContract = Pull { bcActivation :: String
                               , bcDataRequirement :: String
                               , bcEmission :: Emission
                               }
                        | Push { bcActivation :: String
                               , bcDataRequirement :: String
                               , bcEmission :: Emission
                               }
                          deriving Show

data Emission = EmitEmpty | EmitSelf | EmitSelfOpt
              deriving Show
  
-- Lexer
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

-- Parser
document :: Parser Document
document = do { contexts <- many context
              ; return (Document contexts)
              }

dataTypeRef :: Parser DataType
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
          
parseDocument :: String -> Either ParseError Document
parseDocument s = runLex document s

runLex :: Show a => Parser a -> String -> Either ParseError a
runLex p input
  = runParser ( do { whiteSpace
                   ; x <- p
                   ; eof
                   ; return x
                   }) () "" input

-- Sample expressions

mainParse = parseDocument "\
\context Tre as Boolean {<push D; val; empty>}\
\context Tre as Boolean {<pull self; val; empty>}"

-- Generator

data CompilationUnit = CompilationUnit { cuName :: String
                                       , cuContent :: String
                                       }

generateCompilationUnits :: Document -> [CompilationUnit]
generateCompilationUnits doc@(Document ctxts) = generateCUs  doc ctxts

-- how can I make this function hidden inside
-- generateCompilationUnits? I tried with 'where' but failed
generateCUs :: Document -> [Context] -> [CompilationUnit]
generateCUs doc (car : cdr)  = transform doc car : generateCUs doc cdr
generateCUs doc [] = []

transform :: Document -> Context -> CompilationUnit
transform doc ctxt = CompilationUnit "" ""