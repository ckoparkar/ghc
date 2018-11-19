{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ResponseFile
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  portable
--
-- GCC style response files.
--
-- @since 4.12.0.0
----------------------------------------------------------------------------

-- Migrated from Haddock.

module GHC.ResponseFile (
    getArgsWithResponseFiles,
    unescapeArgs,
    escapeArgs,
    expandResponse
  ) where

import Control.Exception
import Data.Char          (isSpace)
import Data.Foldable      (foldl', foldlM)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO

{-|
Like 'getArgs', but can also read arguments supplied via response files.


For example, consider a program @foo@:

@
main :: IO ()
main = do
  args <- getArgsWithResponseFiles
  putStrLn (show args)
@


And a response file @args.txt@:

@
--one 1
--\'two\' 2
--"three" 3
@

Then the result of invoking @foo@ with @args.txt@ is:

> > ./foo @args.txt
> ["--one","1","--two","2","--three","3"]

-}
getArgsWithResponseFiles :: IO [String]
getArgsWithResponseFiles = do
  args <- getArgs
  -- See Trac #15732.
  if rtsSupportsResponseFiles
    then return args
    else (expandResponse args)

foreign import ccall unsafe rtsSupportsResponseFiles :: Bool

-- | Given a string of concatenated strings, separate each by removing
-- a layer of /quoting/ and\/or /escaping/ of certain characters.
--
-- These characters are: any whitespace, single quote, double quote,
-- and the backslash character.  The backslash character always
-- escapes (i.e., passes through without further consideration) the
-- character which follows.  Characters can also be escaped in blocks
-- by quoting (i.e., surrounding the blocks with matching pairs of
-- either single- or double-quotes which are not themselves escaped).
--
-- Any whitespace which appears outside of either of the quoting and
-- escaping mechanisms, is interpreted as having been added by this
-- special concatenation process to designate where the boundaries
-- are between the original, un-concatenated list of strings.  These
-- added whitespace characters are removed from the output.
--
-- > unescapeArgs "hello\\ \\\"world\\\"\n" == escapeArgs "hello \"world\""
unescapeArgs :: String -> [String]
unescapeArgs = filter (not . null) . unescape

-- | Given a list of strings, concatenate them into a single string
-- with escaping of certain characters, and the addition of a newline
-- between each string.  The escaping is done by adding a single
-- backslash character before any whitespace, single quote, double
-- quote, or backslash character, so this escaping character must be
-- removed.  Unescaped whitespace (in this case, newline) is part
-- of this "transport" format to indicate the end of the previous
-- string and the start of a new string.
--
-- While 'unescapeArgs' allows using quoting (i.e., convenient
-- escaping of many characters) by having matching sets of single- or
-- double-quotes,'escapeArgs' does not use the quoting mechasnism,
-- and thus will always escape any whitespace, quotes, and
-- backslashes.
--
-- > unescapeArgs "hello\\ \\\"world\\\"\\n" == escapeArgs "hello \"world\""
escapeArgs :: [String] -> String
escapeArgs = unlines . map escapeArg

-- | Response files can be recursive. E.g. a file 'A' can refer to another
-- file 'B', and so on. If we're not careful, response files can cause an
-- infinite loop (e.g. file 'A' that contains a self-reference). To avoid
-- this, we limit the number of response files that we parse.
rfIterationLimit :: Int
rfIterationLimit = 2000
-- Should match rf_iteration_limit in 'RtsFlags.c'.
-- Also see Trac #15732.

-- | Arguments which look like '@foo' will be replaced with the
-- contents of file @foo@. A gcc-like syntax for response files arguments
-- is expected.  This must re-constitute the argument list by doing an
-- inverse of the escaping mechanism done by the calling-program side.
--
-- We quit if the file is not found or reading somehow fails.
expandResponse :: [String] -> IO [String]
expandResponse args =
  do let (to_expand, ignored) = break (== "--") args
     (_, expanded) <- foldlM (\(lim,acc) arg ->
                                do (lim',arg') <- expand lim arg
                                   return (lim', acc ++ arg'))
                        (rfIterationLimit, []) to_expand
     return (expanded ++ ignored)
  where
    expand :: Int -> String -> IO (Int, [String])
    expand 0 _ = error "Too many @-files encountered."
    expand lim ('@':f) = do
      rf_args <- readFileExc f >>= return . unescapeArgs
      foldlM (\(lim',acc) arg ->
                do (lim'',arg') <- expand (lim'-1) arg
                   return (lim'', acc ++ arg'))
        (lim, []) rf_args
    expand lim arg = return (lim, [arg])

    readFileExc f =
      readFile f `catch` \(e :: IOException) -> do
        hPutStrLn stderr $ "Error while expanding response file: " ++ show e
        exitFailure

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse . map reverse $ go args NoneQ False [] []
    where
      -- n.b., the order of these cases matters; these are cribbed from gcc
      -- case 1: end of input
      go []     _q    _bs   a as = a:as
      -- case 2: back-slash escape in progress
      go (c:cs) q     True  a as = go cs q     False (c:a) as
      -- case 3: no back-slash escape in progress, but got a back-slash
      go (c:cs) q     False a as
        | '\\' == c              = go cs q     True  a     as
      -- case 4: single-quote escaping in progress
      go (c:cs) SngQ  False a as
        | '\'' == c              = go cs NoneQ False a     as
        | otherwise              = go cs SngQ  False (c:a) as
      -- case 5: double-quote escaping in progress
      go (c:cs) DblQ  False a as
        | '"' == c               = go cs NoneQ False a     as
        | otherwise              = go cs DblQ  False (c:a) as
      -- case 6: no escaping is in progress
      go (c:cs) NoneQ False a as
        | isSpace c              = go cs NoneQ False []    (a:as)
        | '\'' == c              = go cs SngQ  False a     as
        | '"'  == c              = go cs DblQ  False a     as
        | otherwise              = go cs NoneQ False (c:a) as

escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs
