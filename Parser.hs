module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import RawSyntax

languageDef =
    emptyDef { Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = [ "val"
                                      , "def"
                                      , "type"
                                      , "new"
                                      , "subtype"
                                      , "extends"
                                      , "Unit"
                                      , "Bot"
                                      ]
            , Token.reservedOpNames = [ "="
                                      , "<="
                                      , "="
                                      , ">="
                                      , "+"
                                      , "."
                                      , ":"
                                      ]
    }
              
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
braces     = Token.parens lexer
integer    = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
dot        = Token.reservedOp lexer "."
colon      = Token.reservedOp lexer ":"

--parser definition
parseProgram :: Parser Program
parseProgram = whiteSpace >> parseProgram'
    where parseProgram' = (\x y -> Program x y) <$> many decl <*> expr

decl = undefined

path :: Parser Path
path = chainl1 (Var <$> identifier) ((\p (Var f) -> Field p f) <$ dot)

expr = try call
   <|> primary

   where 
new = New <$ reserved "new" 
  <*> identifier 
  <*  colon 
  <*> ty 
  <*> braces (many decl)

call = Call <$> path <*> parens (many1 path)

primary = PathExpr <$> path
      <|> UnitLit <$ reserved "Unit"
      <|> (\x -> IntLit $ fromIntegral x) <$> integer
      <|> new
      <|> parens expr

ty = (\x -> Type x []) <$> basetype
 <|> Type <$> basetype <*> braces (many1 decl)

basetype = UnitType <$ reserved "Unit"
       <|> BotType  <$ reserved "Bot"  
       <|> PathType <$> path

