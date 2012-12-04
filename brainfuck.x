{
module Main(main) where
import Data.Array.MArray
import Data.Array.IO
import Data.Char
import System.Environment
}
%wrapper "monadUserState"

tokens :-
                                \>                              { emit MOV_RIGHT }
                                \<                              { emit MOV_LEFT }
                                \.                              { emit PRNT }
                                \+                              { emit INCR }
                                \-                              { emit DECR }
                                \,                              { emit INPUT }
                                \[                              { emit JMPZ }
                                \]                              { emit JMPNZ }
                                $white                          { skip }

{

emit :: Token -> (AlexPosn, Char, String) -> Int -> Alex Token
emit token (_,_,input) len = do
  return token

data Token = MOV_RIGHT
           | MOV_LEFT
           | PRNT
           | INCR
           | DECR
           | INPUT
           | JMPZ
           | JMPNZ
           | EOF
           deriving Eq

data AlexUserState = AlexUserState {
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                    }

alexEOF = return EOF

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                          EOF -> return $ (reverse (EOF:toks))
                          _ -> let foo = loop (tok : toks) in foo
  loop []

main :: IO ()
main = do
  filename <- getArgs >>= return . head
  contents <- readFile filename
  tape <- newArray (0, 1024) 0
  case (scanner contents) of
    Left message -> print message
    Right tokens -> do tokenArr <- newListArray (0, (length tokens)) tokens
                       interp tape 0 tokenArr 0

type Tape = IOArray Int Int
type Tokens = IOArray Int Token

interp :: Tape -> Int -> Tokens -> Int -> IO ()
interp tape tape_idx tokens token_idx = do
  token <- readArray tokens token_idx
  case token of
    MOV_RIGHT -> interp tape (tape_idx+1) tokens (token_idx+1)
    MOV_LEFT -> interp tape (tape_idx-1) tokens (token_idx+1)
    PRNT -> readArray tape tape_idx >>= (putChar . chr) >> interp tape tape_idx tokens (token_idx+1)
    INCR -> readArray tape tape_idx >>= \val -> writeArray tape tape_idx (val+1) >> interp tape tape_idx tokens (token_idx+1)
    DECR -> readArray tape tape_idx >>= \val -> writeArray tape tape_idx (val-1) >> interp tape tape_idx tokens (token_idx+1)
    JMPZ -> readArray tape tape_idx >>= \val -> if val == 0 then (jmpz tokens token_idx) >>= \new_idx -> interp tape tape_idx tokens new_idx else interp tape tape_idx tokens (token_idx+1)
    JMPNZ -> readArray tape tape_idx >>= \val -> if val /= 0 then (jmpnz tokens token_idx) >>= \new_idx -> interp tape tape_idx tokens new_idx else interp tape tape_idx tokens (token_idx+1)
    EOF -> return ()

jmpz :: Tokens -> Int -> IO Int
jmpz tokens idx =do
  val <- readArray tokens idx
  if val == JMPNZ then return (idx+1) else jmpz tokens (idx+1)

jmpnz :: Tokens -> Int -> IO Int
jmpnz tokens idx =do
  val <- readArray tokens idx
  if val == JMPZ then return idx else jmpnz tokens (idx-1)

}
