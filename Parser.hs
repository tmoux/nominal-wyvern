module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import RawSyntax

languageDef =
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
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
                                      , ","
                                      ]
    }
              
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
integer    = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
dot        = Token.reservedOp lexer "."
colon      = Token.reservedOp lexer ":"
comma      = Token.reservedOp lexer ","

--parser definition
parseProgram :: Parser Program
parseProgram = whiteSpace >> parseProgram'
    where parseProgram' = (\x y -> Program x y) <$> many decl <*> expr

--declarations
decl :: Parser Declaration
decl = try valdecl 
   <|> try defdecl
   <|> try typedecl
   <|> try typeEqDecl
   <|> try subtypedecl

valdecl = ValDecl <$  reserved "val"
                  <*> identifier 
                  <*  reservedOp "="
                  <*> expr

defdecl = DefDecl <$  reserved "def"
                  <*> identifier
                  <*> parens (((,) <$> identifier <* colon <*> ty) `sepBy` comma)
                  <*  colon
                  <*> ty
                  <*> braces parseProgram

typedecl = TypeDecl <$  reserved "type"
                    <*> identifier
                    <*  colon
                    <*> identifier
                    <*> braces (many refine)

typeEqDecl = TypeEqDecl <$  reserved "type"
                <*> identifier
                <*  reservedOp "="
                <*> ty

subtypedecl = SubtypeDecl <$  reserved "subtype"
                          <*> ty
                          <*  reserved "extends"
                          <*> ty
--refinements
refine = try valref
     <|> try defref
     <|> try typeref
     <|> try memberref
     <|> try subtyperef

valref = ValRef <$  reserved "val"
                <*> identifier
                <*  colon
                <*> ty

defref = DefRef <$  reserved "def"
                <*> identifier
                <*> parens (((,) <$> identifier <* colon <*> ty) `sepBy` comma)
                <*  colon
                <*> ty

typeref = TypeRef <$  reserved "type"
                  <*> identifier
                  <*  colon
                  <*> identifier
                  <*> braces (many refine)

memberref = MemberRef <$  reserved "type"
                      <*> identifier
                      <*> bound
                      <*> ty

subtyperef = SubtypeRef <$  reserved "subtype"
                        <*> ty
                        <*  reserved "extends"
                        <*> ty

bound :: Parser Bound
bound = LEQ <$ reservedOp "<="
    <|> EQQ <$ reservedOp "="
    <|> GEQ <$ reservedOp ">="

--expressions
path :: Parser Path
path = chainl1 (Var <$> identifier) ((\p (Var f) -> Field p f) <$ dot)

expr = try call
   <|> try primary

call = Call <$> path <*> parens (path `sepBy` comma)

primary = PathExpr <$> path
      <|> UnitLit <$ reserved "Unit"
      <|> (\x -> IntLit $ fromIntegral x) <$> integer
      <|> new
      <|> parens expr

new = New <$ reserved "new" 
  <*> identifier 
  <*  colon 
  <*> ty 
  <*> braces (many decl)

--types
ty = (\x -> Type x []) <$> basetype
 <|> Type <$> basetype <*> braces (refine `sepBy` comma)

basetype = UnitType <$ reserved "Unit"
       <|> BotType  <$ reserved "Bot"  
       <|> PathType <$> path
