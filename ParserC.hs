{-
 - Parser combinators
 -}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE NamedFieldPuns,
             RankNTypes,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
module ParserC (
    (+++),
    (<$>),
    (<?>),
    (<|>),
    (|:>),
    (||>),
    ParserC,
    ParserError(..),
    ParserState(..),
    after,
    anyOf,
    between,
    brackets,
    chainl1,
    chainr1,
    char,
    digit,
    eof,
    item,
    letter,
    lower,
    many,
    manyTill,
    someTill,
    noneOf,
    number,
    offsetAfter,
    runP,
    execP,
    sat,
    sepBy,
    sepBy1,
    sepByOpt,
    some,
    special,
    string,
    throwParserError,
    try,
    upper,
    whitespace,
    word,
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate, nub)

type Location = (Int, Int)
type Expected = String
type Got      = String

data ParserError
    = PErr (Maybe Location) [Expected] Got
    -- | PErrString Location String
    -- | PErr String String
    | PErrIndent Location
    | EOFErr

errloc :: ParserError -> Maybe Location
errloc (PErr l _ _)   = l
errloc (PErrIndent l) = Just l
errloc EOFErr         = Nothing

-- TODO: should collect all errors end at the end, pick the farthest one
throwParserError :: ParserError -> ParserC ext a
throwParserError (PErr Nothing s1 s2)
    = do ParserState{location} <- get
         throwError $ traceShowId $ PErr (Just location) s1 s2
    where traceShowId = id
throwParserError err
    = throwError err

-- TODO: this is hackish
instance Monoid ParserError where
    mempty
        = PErr Nothing [""] ""
    mappend (PErr (Just l1) exp1 g1) (PErr (Just l2) exp2 g2)
        | l1 == l2  = PErr (Just l1) (exp1 ++ exp2) g1
        | l1 <  l2  = PErr (Just l2) exp2 g2
        | otherwise = PErr (Just l1) exp1 g1
    -- TODO: this is not correct if there are other types of errors
    mappend _ e = e

parserErrorPrefix :: String
parserErrorPrefix = "Parse error"

instance Show ParserError where
    show EOFErr
        = parserErrorPrefix ++ ". Unexpected end of file"
    show (PErrIndent (l, c))
        = parserErrorPrefix ++ " on line " ++ loc'
            ++ ": unexpected indentation"
        where loc' = show l ++ ":" ++ show c
    show (PErr (Just (l, c)) exp' found)
        = parserErrorPrefix ++ " on line " ++ loc' ++ case found of
            []    -> ", expected: " ++ show expl
            f     -> " on input '" ++ f ++ "', expected: '" ++ expl ++ "'"
        where loc' = show l ++ ":" ++ show c
              expl = intercalate " or " (nub exp')
    show PErr{}
        = undefined

data ParserState a = ParserState {
    remaining  :: String,
    location   :: Location,
    indent     :: Int,
    reserved   :: [String],
    extension  :: a
} deriving (Show)

newtype ParserC ext a
    = ParserC { unP :: ExceptT ParserError (State (ParserState ext)) a }
    deriving (Functor,
              Applicative,
              Alternative,
              Monad,
              MonadState (ParserState ext),
              MonadError ParserError)

runP :: ParserC ext a -> ParserState ext -> (Either ParserError a, ParserState ext)
runP = runState . runExceptT . unP

execP :: ParserC ext a -> ParserState ext -> Either ParserError a
execP = evalState . runExceptT . unP

loc :: ParserC ext Location
loc = location <$> get

withLoc :: (Location -> ParserC ext b) -> ParserC ext b
withLoc p
    = loc >>= \(r, c) -> p (r, c + 1)

---- Combinators ---------------------------------------------------------------

(|:>) :: ParserC ext a -> ParserC ext [a] -> ParserC ext [a]
(|:>)
    = liftA2 (:)

(||>) :: Monoid a => ParserC ext a -> ParserC ext a -> ParserC ext a
(||>)
    = liftA2 mappend

-- | Try to run parser and backtrack if failed
try :: ParserC ext a -> ParserC ext a
try p = do
    st <- get
    p `catchError` \e ->
        put st >>
        throwParserError e

(<?>) :: String -> ParserC ext a -> ParserC ext a
expected <?> p = do
    st <- get
    case runP p st of
        (Right r, state') -> do
            put state'
            return r
        (Left err, state') ->
            if pl (errloc err) == (Just $ location st)
                then throwParserError (PErr (Just $ location st) [expected] "")
                else put state' >> throwParserError err
    where pl (Just (r, l)) = Just (r, l - 1)
          pl Nothing       = Nothing

infixl 3 +++
(+++) :: ParserC ext a -> ParserC ext a -> ParserC ext a
(+++) p1 p2
    = try p1 <|> p2

item :: ParserC ext Char
item = do
    s <- get
    let (row, col) = location s
    case remaining s of
      (x : xs) -> do
        let newloc = case x of
                    '\n' -> (row + 1, 0)
                    _    -> (row, col + 1)
        if snd newloc <= indent s && x `notElem` (" \n\t" :: String)
            then throwParserError $ PErrIndent newloc
            else put s{remaining = xs, location = newloc} >> return x
      []       -> throwParserError EOFErr

eof :: ParserC ext ()
eof = do
    s <- get
    let l = location s
    case remaining s of
        [] -> return ()
        r  -> throwParserError $ PErr (Just l) ["End of file"] r

sat :: (Char -> Bool) -> (String -> ParserError) -> ParserC ext Char
sat p err = try $ do
    x <- item
    if p x
        then return x
        else throwParserError (err [x])

char :: Char -> ParserC ext Char
char c = do
    let err = PErr Nothing [[c]]
    sat (== c) err

string :: String -> ParserC ext String
string s = withLoc $ \loc' ->
    string' s
    `catchError` \err -> case err of
        PErr{} -> throwParserError $ PErr (Just loc') [s] ""
        _      -> throwParserError err
    where string' (c : cs) = do
            x  <- char c
            xs <- string' cs
            return (x : xs)
          string' [] = return []

anyOf :: String -> ParserC ext Char
anyOf xs = sat (`elem` xs')
               (PErr Nothing (map (:[]) xs'))
    where xs' = nub xs

noneOf :: String -> ParserC ext Char
noneOf xs = sat (`notElem` xs')
                (PErr Nothing [])
    where xs' = nub xs

digit :: ParserC ext Char
digit = sat (`elem` ['0'..'9'])
        (PErr Nothing ["digit"])

number :: ParserC ext Int
number = do sign <- try (Just <$> char '-')
                    <|> try (Just <$> char '+')
                    <|> return Nothing
            zs <- many (char '0')
            n <- many digit
            case zs ++ n of
              [] -> throwParserError $ PErr Nothing ["number"] ""
              _  -> let n' = read (zs ++ n) :: Int in
                    if (n == show n' || null n) && n' <= maxSize sign
                    then case sign of
                        Just '-'  -> return $ (-1) * n'
                        _         -> return n'
                    else throwParserError $ PErr Nothing ["int"] "big int"
        where maxSize (Just '-') = 2 ^ (31 :: Int)
              maxSize _          = 2 ^ (31 :: Int) - 1

upper :: ParserC ext Char
upper = sat (\c -> 'A' <= c && c <= 'Z')
            (PErr Nothing ["uppercase letter"])

lower :: ParserC ext Char
lower = sat (\c -> 'a' <= c && c <= 'z')
            (PErr Nothing ["lowercase letter"])

letter :: ParserC ext Char
letter = lower +++ upper

special :: ParserC ext Char
special = sat (`elem` ("&~!@#$%^*-=_+?<>,/?;:" :: String))
          (PErr Nothing ["special character"])

word :: ParserC ext String
word = some letter

whitespace :: ParserC ext Char
whitespace = anyOf " \n\t"

manyTill :: ParserC ext a -> ParserC ext b -> ParserC ext [b]
manyTill term p = do
    st <- get
    case runP term st of
        (Right _, state') -> do
            put state'
            return []
        _ -> p |:> manyTill term p

someTill :: ParserC ext a -> ParserC ext b -> ParserC ext [b]
someTill term p
    = p |:> manyTill term p

sepBy1 :: ParserC ext a -> ParserC ext b -> ParserC ext [b]
sepBy1 pa pb = do
    b  <- pb
    bs <- many . try $ (pa >> pb)
    return $ b : bs

sepBy :: ParserC ext a -> ParserC ext b -> ParserC ext [b]
sepBy pa pb
    = try (sepBy1 pa pb) <|> return []

between :: ParserC ext a -> ParserC ext b -> ParserC ext c -> ParserC ext c
between open close p = do
    open
    p' <- p
    close
    return p'

after :: ParserC ext a -> ParserC ext b -> ParserC ext b
after a p = a >> p

brackets :: Char -> Char -> ParserC ext a -> ParserC ext a
brackets open close
    = between (char open >> many whitespace) (many whitespace >> char close)

chainl1 :: ParserC ext a -> ParserC ext (a -> a -> a) -> ParserC ext a
chainl1 p op = do
    x <- p
    rest x
    where rest x = do{
        f <- op;
        y <- p;
        rest (f x y)} +++ return x

chainr1 :: ParserC ext a -> ParserC ext (a -> a -> a) -> ParserC ext a
chainr1 p op
    = scan
    where
      scan   = do{ x <- p; rest x }
      rest x = do{ f <- op
                 ; y <- scan
                 ; return (f x y)
                 }
             <|> return x

sepByOpt :: ParserC ext a -> ParserC ext b -> ParserC ext [b]
sepByOpt pa pb = sepBy pa pb <|> return []

-- Like (>>=) , but only parses 'p' to the right hand side of
-- 'a'
offsetAfter :: ParserC ext a -> (a -> ParserC ext b) -> ParserC ext b
offsetAfter a p = do
    st <- get
    let _   = snd (location st)
        ind = indent st
    d <- (a' >>= p) `catchError` (\err -> restore ind >> throwError err)
    restore ind
    return d
    where restore :: Int -> ParserC ext ()
          restore ind = do
            st <- get
            put st{ indent = ind }
          a' = do
               st <- get
               res <- a
               let col = snd (location st)
               st' <- get
               put st' { indent = col + 1 }
               return res
