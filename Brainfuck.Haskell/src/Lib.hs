{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseRun
    ) where

import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.Char (ord, chr)
import           Data.Text (Text, pack)
import           Data.Void
import           Data.Word (Word8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Tape a = Tape [a] a [a]

data Instruction =
    Inc           |
    Dec           |
    Forwards      | 
    Backwards     |
    Read          |
    Write         |
    Loop [Instruction]
    deriving Show

type Parser = Parsec Void Text
type Brainfuck = StateT (Tape Word8) IO

parseRun :: String -> IO ()
parseRun inp = case parse (program <* eof) "bf" (pack . ignoreComments $ inp) of
        Left err -> print err
        Right ins -> run ins >> mempty

run :: [Instruction] -> IO (Tape Word8)
run = flip execStateT defaultState . mapM_ eval
        
defaultState :: Tape Word8
defaultState = Tape zeros 0 zeros
    where zeros = repeat 0

forwards :: Tape a -> Tape a
forwards (Tape ls x (r:rs) ) = Tape (x:ls) r rs

backwards :: Tape a -> Tape a
backwards (Tape (l:ls) x rs ) = Tape ls l (x:rs)

modifyVal :: (a -> a) -> Tape a -> Tape a
modifyVal f (Tape ls x rs) = Tape ls (f x) rs

getVal :: Tape a -> a
getVal (Tape _ x _) = x

eval :: Instruction -> Brainfuck ()
eval Inc = modify $! modifyVal (+ 1)
eval Dec = modify $! modifyVal (+ (- 1))
eval Forwards = modify forwards
eval Backwards = modify backwards
eval Write = gets getVal >>= liftIO . putChar . chr . fromEnum
eval Read = liftIO getChar >>= modify . modifyVal . const . toEnum . ord
eval loop@(Loop inner) = gets getVal >>= exec
    where exec 0 = return ()
          exec _ = mapM_ eval inner >> gets getVal >>= exec

ignoreComments :: String -> String
ignoreComments = filter (`elem` (",.<>+-[]" :: String))

program :: Parser [Instruction]
program = many $ loop <|> instr

symbol :: Text -> Parser Text
symbol = L.symbol space

instr :: Parser Instruction
instr = build <$> instrChar
    where build '+' = Inc
          build '-' = Dec
          build '>' = Forwards
          build '<' = Backwards
          build '.' = Write
          build ',' = Read

instrChar :: Parser Char
instrChar = char '+' <|>
            char '<' <|>
            char '>' <|>
            char '-' <|>
            char '.' <|>
            char ',' 

loop :: Parser Instruction
loop = Loop <$> between (symbol "[") (symbol "]") program