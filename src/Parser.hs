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
             , Token.identStart     = letter <|> char '_'
             , Token.identLetter    = alphaNum <|> char '_'
             , Token.reservedNames  = [ "val"
                                      , "def"
                                      , "type"
                                      , "new"
                                      , "subtype"
                                      , "extends"
                                      , "Unit"
                                      , "Bot"
                                      , "@shape"
                                      , "quit"
                                      , "reset"
                                      , "query"
                                      ]
            , Token.reservedOpNames = [ "="
                                      , "<="
                                      , "="
                                      , ">="
                                      , "+"
                                      , "."
                                      , ":"
                                      , ","
                                      , "{"
                                      , "}"
                                      , "=>"
                                      , "<:"
                                      ]
    }
              
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
resOp      = Token.reservedOp lexer
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
decl = try valAnnotDecl
   <|> try defdecl
   <|> try typedecl
   <|> try typeEqDecl
   <|> try subtypedecl

{-
valdecl = ValDecl <$  reserved "val"
                  <*> identifier 
                  <*  resOp "="
                  <*> expr
-}

valAnnotDecl = ValAnnotDecl <$  reserved "val"
                            <*> identifier
                            <*  colon
                            <*> ty
                            <*  resOp "="
                            <*> expr

defdecl = DefDecl <$  reserved "def"
                  <*> identifier
                  <*> parens (((,) <$> identifier <* colon <*> ty) `sepBy` comma)
                  <*  colon
                  <*> ty
                  <*> braces parseProgram

typeAnnot = f <$> optionMaybe (reserved "@shape")
  where f (Just _) = Shape
        f Nothing  = Material

typedecl = TypeDecl <$> typeAnnot
                    <*  reserved "type"
                    <*> identifier
                    <*  resOp "{" 
                    <*> identifier <* resOp "=>"
                    <*> many refine
                    <*  resOp "}"

typeEqDecl = TypeEqDecl <$  reserved "type"
                <*> identifier
                <*  resOp "="
                <*> ty

subtypedecl = SubtypeDecl <$  reserved "subtype"
                          <*> ty
                          <*  reserved "extends"
                          <*> basetype
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

typeref = TypeRef <$> typeAnnot 
                  <*  reserved "type"
                  <*> identifier
                  <*  resOp "{"
                  <*> identifier <* resOp "=>"
                  <*> many refine
                  <*  resOp "}"

memberref = MemberRef <$> typeAnnot
                      <*  reserved "type"
                      <*> identifier
                      <*> bound
                      <*> ty

subtyperef = SubtypeRef <$  reserved "subtype"
                        <*> ty
                        <*  reserved "extends"
                        <*> basetype

bound :: Parser Bound
bound = LEQ <$ resOp "<="
    <|> EQQ <$ resOp "="
    <|> GEQ <$ resOp ">="

--expressions
path :: Parser Path
path = chainl1 (Var <$> identifier) ((\p (Var f) -> Field p f) <$ dot)

expr = try call
   <|> try add
   <|> try primary

call = Call <$> path <*> parens (path `sepBy` comma)

add = addCall <$> path <* resOp "+" <*> path
  where addCall = \a b -> Call (Field a "plus") [b]

primary = PathExpr <$> path
      <|> UnitLit <$ reserved "Unit"
      <|> (\x -> IntLit $ fromIntegral x) <$> integer
      <|> new
      <|> parens expr

new = New <$ reserved "new" 
  <*> ty
  <*  resOp "{"
  <*> identifier <* resOp "=>"
  <*> many decl
  <*  resOp "}"

--types
ty = try (Type <$> basetype <*> braces (refine `sepBy` comma))
     <|> (\x -> Type x []) <$> basetype


basetype = UnitType <$ reserved "Unit"
       <|> BotType  <$ reserved "Bot"  
       <|> PathType <$> path


--repl parser
data ReplLine = 
    Quit
  | Reset
  | ReplDecl Declaration
  | ReplExpr Expr
  | Query Type Type
  deriving (Show)

parseRepl = Quit <$ reserved "quit"
        <|> Reset <$ reserved "reset"
        <|> ReplDecl <$> decl
        <|> ReplExpr <$> expr
        <|> Query <$ reserved "query" <*> ty <* resOp "<:" <*> ty
