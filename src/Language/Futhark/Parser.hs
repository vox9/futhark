-- | Interface to the Futhark parser.
module Language.Futhark.Parser
  ( parseFuthark
  , parseExp
  , parseType
  , parseLambda

  , parseValue
  , parseValues

  , parseExpIncr
  , parseExpIncrIO

  , ParseError (..)
  , RealConfiguration (..)
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List (intersect)

import Prelude

import Language.Futhark.Syntax
import Language.Futhark.Attributes hiding (arrayValue)
import Language.Futhark.Parser.Parser
import Language.Futhark.Parser.Lexer
import Language.Futhark.Parser.RealConfiguration

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> RealConfiguration -> FilePath -> String
             -> ReadLineMonad (Either ParseError a)
parseInMonad p rconf file program =
  either (Left . ParseError) Right <$> either (return . Left)
  (evalStateT (runReaderT (runExceptT p) env))
  (alexScanTokens file program)
  where env = case rconf of RealAsFloat32 ->
                              ParserEnv file Float32 toFloat32 float32funs
                            RealAsFloat64 ->
                              ParserEnv file Float64 Float64Value float64funs
        toFloat32 x =
          let (m,n) = decodeFloat x
          in Float32Value $ encodeFloat m n
        float32funs = HM.map (<>nameFromString "32") funs
        float64funs = HM.map (<>nameFromString "64") funs
        funs = HM.fromList $ zip funnames funnames
        funnames = map nameFromString ["sqrt", "log", "exp", "sin", "cos"]

parseIncrementalIO :: ParserMonad a -> RealConfiguration -> FilePath -> String
                   -> IO (Either ParseError a)
parseIncrementalIO p rconf file program =
  getLinesFromIO $ parseInMonad p rconf file program

parseIncremental :: ParserMonad a -> RealConfiguration -> FilePath -> String
                 -> Either ParseError a
parseIncremental p rconf file program =
  either (Left . ParseError) id
  $ getLinesFromStrings (lines program)
  $ parseInMonad p rconf file ""

parse :: ParserMonad a -> RealConfiguration -> FilePath -> String
      -> Either ParseError a
parse p rconf file program =
  either (Left . ParseError) id
  $ getNoLines $ parseInMonad p rconf file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: RealConfiguration -> FilePath -> String
             -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: RealConfiguration -> FilePath -> String
               -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

-- | Parse an entire Futhark program from the given 'String', using the
-- 'FilePath' as the source name for error messages, and parsing and reacting to
-- all headers.
parseFuthark :: RealConfiguration -> FilePath -> String
                -> IO (Either ParseError UncheckedProg)
parseFuthark rc fp0 s0 = parseWithPrevIncludes [fp0] (fp0, s0)
  where parseWithPrevIncludes :: [FilePath] -> (FilePath, String)
                              -> IO (Either ParseError UncheckedProg)
        parseWithPrevIncludes prevIncludes (fp, s) =
          case parse prog rc fp s of
            Left e -> return $ Left e
            Right p ->
              let newIncludes = mapMaybe headerInclude $ progWHHeaders p
                  intersection = prevIncludes `intersect` newIncludes
              in if not (null intersection)
                 then return $ Left $ ParseError
                      ("Include cycle with " ++ show intersection ++ ".")
                 else let p' = Prog $ progWHFunctions p
                      in if null newIncludes
                         then return $ Right p'
                         else includeIncludes prevIncludes newIncludes p'

        includeIncludes :: [FilePath] -> [FilePath] -> UncheckedProg
                           -> IO (Either ParseError UncheckedProg)
        includeIncludes prevIncludes newIncludes endProg = do
          let allIncludes = prevIncludes ++ newIncludes
          ss <- liftIO $ mapM readFile newIncludes
          parses <- liftIO $ mapM (parseWithPrevIncludes allIncludes)
            (zip newIncludes ss)
          return $ foldr mergePrograms (Right $ endProg) parses

        mergePrograms :: Either ParseError UncheckedProg
                      -> Either ParseError UncheckedProg
                      -> Either ParseError UncheckedProg
        mergePrograms a b = case (a, b) of
          (Right (Prog fs), Right (Prog gs)) -> Right (Prog (fs ++ gs))
          (Left err, _) -> Left err
          (_, Left err) -> Left err

        headerInclude :: ProgHeader -> Maybe String
        headerInclude (Include name) = Just name

-- | Parse an Futhark expression from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseExp :: RealConfiguration -> FilePath -> String
         -> Either ParseError UncheckedExp
parseExp = parse expression

-- | Parse an Futhark type from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseType :: RealConfiguration -> FilePath -> String
          -> Either ParseError UncheckedType
parseType = parse futharktype

-- | Parse an Futhark anonymous function from the given 'String', using the
-- 'FilePath' as the source name for error messages.
parseLambda :: RealConfiguration -> FilePath -> String
            -> Either ParseError UncheckedLambda
parseLambda = parse lambda

-- | Parse any Futhark value from the given 'String', using the 'FilePath'
-- as the source name for error messages.
parseValue :: RealConfiguration -> FilePath -> String
           -> Either ParseError Value
parseValue = parse anyValue

-- | Parse several Futhark values (separated by anything) from the given
-- 'String', using the 'FilePath' as the source name for error
-- messages.
parseValues :: RealConfiguration -> FilePath -> String
            -> Either ParseError [Value]
parseValues = parse anyValues
