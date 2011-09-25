{-# LANGUAGE
    TemplateHaskell
  , CPP #-}

module System.ShQQ
    ( sh
    , shc
    , readShell
    , readShellWithCode
    , showNonString
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Applicative
import Control.Exception ( evaluate, throwIO )
import Data.Char
import Data.Foldable ( asum )
import Data.Typeable ( Typeable, cast )
import Text.Parsec hiding ( (<|>), many )
import Text.Parsec.String
import System.IO
import System.Exit

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

readShellWithCode :: String -> IO (ExitCode, String)
readShellWithCode cmd = do
    (Nothing, Just hOut, Nothing, hProc) <- P.createProcess $
        (P.shell cmd) { P.std_out = P.CreatePipe }
    out <- hGetContents hOut
    _   <- evaluate (length out)
    hClose hOut
    ec  <- P.waitForProcess hProc
    return (ec, out)

readShell :: String -> IO String
readShell cmd = do
    (ec, out) <- readShellWithCode cmd
    case ec of
        ExitSuccess -> return out
        _ -> throwIO ec

mkExp :: Q Exp -> [Tok] -> Q Exp
mkExp reader toks = [| $reader (concat $strs) |] where
    strs = listE (map f toks)
    var  = varE . mkName
    f (Lit     x) = [| x |]
    f (VarOne  v) = [| E.escape (showNonString $(var v)) |]
    f (VarMany v) = [| showNonString $(var v) |]

shExp :: Q Exp -> String -> Q Exp
shExp reader xs = case parse parseToks "System.ShQQ expression" xs of
    Left  e -> error ('\n' : show e)
    Right t -> mkExp reader t

baseQQ :: QuasiQuoter
baseQQ = QuasiQuoter
    { quoteExp  = error "internal error in System.ShQQ"
    , quotePat  = const (error "no pattern quote for System.ShQQ")
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = const (error "no type quote for System.ShQQ")
    , quoteDec  = const (error "no decl quote for System.ShQQ")
#endif
    }

sh, shc :: QuasiQuoter
sh  = baseQQ { quoteExp = shExp [| readShell |] }
shc = baseQQ { quoteExp = shExp [| readShellWithCode |] }
