module Main where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Control.Applicative hiding ((<|>),many)

-- AST
data Document = Document [Context]
              deriving Show

data DataType = String | Integer | Boolean | UserDT String
                deriving Show
  
data Context = Context { ctName :: String 
                       , ctType :: DataType
                       , ctBehavioralContracts :: [BehavioralContract] }
             deriving Show

data BehavioralContract = Pull { bcDataRequirement :: String
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
angles = P.angles lexer
braces = P.braces lexer
semi = P.semi lexer
identifier= P.identifier lexer
reserved = P.reserved lexer
reservedOp= P.reservedOp lexer

-- Parser
document :: Parser Document
document = Document <$> many context

dataTypeRef :: Parser DataType
dataTypeRef =     (reserved "Boolean" *> return Boolean) 
              <|> (reserved "Integer" *> return Integer)
              <?> "data type"

emission :: Parser Emission
emission =     (reserved "empty" *> return EmitEmpty)
           <|> (reserved "self" *> (option EmitSelf (char '?' *> return EmitSelfOpt)))

behavioralContract :: Parser BehavioralContract
behavioralContract = angles contract
  where contract =     Pull <$> (reserved "pull" *> reserved "self" *> semi *> identifier) 
                            <*> (semi *> emission)
                   <|> Push <$> (reserved "push" *> identifier) 
                            <*> (semi *> identifier) 
                            <*> (semi *> emission)

context :: Parser Context
context = Context <$> (reserved "context" *> identifier) 
                  <*> (reserved "as" *> dataTypeRef) 
                  <*> braces (sepEndBy behavioralContract semi)
          
parseDocument :: String -> Either ParseError Document
parseDocument s = runLex document s

runLex :: Show a => Parser a -> String -> Either ParseError a
runLex p = parse (whiteSpace *> p) ""

-- Sample expressions

mainParse = parseDocument "\
\context Tre as Boolean {<push D; val; empty>}\
\context Tre as Boolean {<pull self; val; empty>}"

-- Generator

data CompilationUnit = CompilationUnit { cuName :: String
                                       , cuContent :: String
                                       }

generateCompilationUnits :: Document -> [CompilationUnit]
generateCompilationUnits doc@(Document ctxts) = map (transform doc) ctxts

transform :: Document -> Context -> CompilationUnit
transform doc ctxt = CompilationUnit "" ""

-- main function : make it compile
main = print mainParse
