{-# LANGUAGE
    TemplateHaskell
  , CPP #-}

module System.ShQQ
    ( sh
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Applicative
import Control.Exception ( evaluate )
import Data.Char
import Data.Foldable ( asum )
import Data.Typeable ( Typeable, cast )
import Text.Parsec hiding ( (<|>), many )
import Text.Parsec.String
import System.IO

import qualified System.Posix.Escape as E
import qualified System.Process      as P

showNonString :: (Typeable a, Show a) => a -> String
showNonString x = case cast x of
    Just y  -> y
    Nothing -> show x

data Tok
    = Lit String
    | VarOne  String
    | VarMany String
    deriving (Show)

parseToks :: Parser [Tok]
parseToks = many part where
    isIdent '_' = True
    isIdent x   = isAlphaNum x
    -- NB:  '\'' excluded

    ident = some (satisfy isIdent)
    var =     VarOne  <$> ident
          <|> VarMany <$  char '+' <*> ident
    part = asum [
        char '\\' *> ( Lit "\\" <$ char '\\'
                  <|>  Lit "$"  <$ char '$' )
        , char '$' *>
            ( var <|> between (char '{') (char '}') var )
      , Lit <$> some (noneOf "$\\") ]

runCmd :: String -> IO String
runCmd cmd = do
    (Nothing, Just hOut, Nothing, hProc) <- P.createProcess $
        (P.shell cmd) { P.std_out = P.CreatePipe }
    out <- hGetContents hOut
    _   <- evaluate (length out)
    hClose hOut
    _   <- P.waitForProcess hProc
    return out

mkExp :: [Tok] -> Q Exp
mkExp toks = [| runCmd (concat $strs) |] where
    strs = listE (map f toks)
    var  = varE . mkName
    f (Lit     x) = [| x |]
    f (VarOne  v) = [| E.escape (showNonString $(var v)) |]
    f (VarMany v) = [| showNonString $(var v) |]

shExp :: String -> Q Exp
shExp xs = case parse parseToks "System.ShQQ expression" xs of
    Left  e -> error ('\n' : show e)
    Right t -> mkExp t

sh :: QuasiQuoter
sh = QuasiQuoter
    { quoteExp  = shExp
    , quotePat  = const (error "no pattern quote for System.ShQQ")
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = const (error "no type quote for System.ShQQ")
    , quoteDec  = const (error "no decl quote for System.ShQQ")
#endif
    }
