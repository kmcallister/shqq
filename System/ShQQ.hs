{-# LANGUAGE
    TemplateHaskell
  , CPP #-}

-- | Embed shell commands with interpolated Haskell
-- variables, and capture output.
module System.ShQQ
    ( -- * Quasiquoters
      sh
    , shc

      -- * Helper functions
      --
      -- | These functions are used in the implementation of
      -- @'sh'@, and may be useful on their own.
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

import qualified System.Posix.Escape.Unicode as E
import qualified System.Process as P

-- | Acts like the identity function on @'String'@, and
-- like @'show'@ on other types.
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

-- | Execute a shell command, capturing output and exit code.
--
-- Used in the implementation of @'shc'@.
readShellWithCode :: String -> IO (ExitCode, String)
readShellWithCode cmd = do
    (Nothing, Just hOut, Nothing, hProc) <- P.createProcess $
        (P.shell cmd) { P.std_out = P.CreatePipe }
    out <- hGetContents hOut
    _   <- evaluate (length out)
    hClose hOut
    ec  <- P.waitForProcess hProc
    return (ec, out)

-- | Execute a shell command, capturing output.
--
-- Used in the implementation of @'sh'@.
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


{- | Execute a shell command, capturing output.

This requires the @QuasiQuotes@ extension.

The expression @[sh| ... |]@ has type @'IO' 'String'@.
Executing this IO action will invoke the quoted shell
command and produce its standard output as a @'String'@.

>>> [sh| sha1sum /proc/uptime |]
"ebe14a88cf9be69d2192dcd7bec395e3f00ca7a4  /proc/uptime\n"

You can interpolate Haskell @'String'@ variables using the
syntax @$x@.  Special characters are escaped, so that the
program invoked by the shell will see each interpolated
variable as a single argument.

>>> let x = "foo bar" in [sh| cat $x |]
cat: foo bar: No such file or directory
*** Exception: ExitFailure 1

You can also write @${x}@ to separate the variable name from
adjacent characters.

>>> let x = "b" in [sh| echo a${x}c |]
"abc\n"

Be careful: the automatic escaping means that @[sh| cat '$x'
|]@ is /less safe/ than @[sh| cat $x |]@, though it will
work \"by accident\" in common cases.

To interpolate /without/ escaping special characters, use
the syntax @$+x@ .

>>> let x = "foo bar" in [sh| cat $+x |]
cat: foo: No such file or directory
cat: bar: No such file or directory
*** Exception: ExitFailure 1

You can pass a literal @$@ to the shell as @\\$@, or a
literal @\\@ as @\\\\@.

As demonstrated above, a non-zero exit code from the
subprocess will raise an exception in your Haskell program.

Variables of type other than @'String'@ are interpolated via
@'show'@.

>>> let x = Just (2 + 2) in [sh| touch $x; ls -l J* |]
"-rw-r--r-- 1 keegan keegan 0 Oct  7 23:28 Just 4\n"

The interpolated variable's type must be an instance of
@'Show'@ and of @'Typeable'@.

-}

sh :: QuasiQuoter
sh = baseQQ { quoteExp = shExp [| readShell |] }


{- | Execute a shell command, capturing output and exit code.

The expression @[shc| ... |]@ has type @'IO' ('ExitCode',
'String')@.  A non-zero exit code does not raise an
exception your the Haskell program.

Otherwise, @'shc'@ acts like @'sh'@.

-}

shc :: QuasiQuoter
shc = baseQQ { quoteExp = shExp [| readShellWithCode |] }
