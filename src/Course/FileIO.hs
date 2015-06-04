{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIo.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
  a <- getArgs
  case a of
    h:._ -> run h
    Nil -> run "/Users/peterson/proj/haskell/nicta-course/share/files.txt"

  -- using bind (>>=) notation ...
  --
  -- getArgs >>= \a ->
  -- case a of
  --   (a:._) -> undefined
  --   Nil -> putStrLn "error: need an argument"


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run file =
  do c <- readFile file
     d <- getFiles (lines c)
     printFiles d

  --  using bind (>>=) notation ...
  --
  -- readFile file >>= \c ->
  -- getFiles (lines c) >>= \d ->
  -- printFiles d


getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles files =
  sequence (getFile <$> files)

--
-- to test:
--
-- getFile "a.txt"
--
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile name =
  (\content -> (name,content)) <$> readFile name


--
-- to test:
-- > printFiles (("name1","contents1") :. ("name2","contents2") :. Nil)
--
-- ======== name1
-- contents1
-- ======== name2
-- contents2
--
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files =
  -- void (sequence ((\(n,c) -> printFile n c) <$> files))

  -- improve this using uncurry ...  (a -> b -> c) -> (a, b) -> c
  void (sequence ((uncurry printFile) <$> files))


--
-- to test:
-- > printFile "name1" "content1"
--
-- ======== name1
-- content1
--
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name content =
  putStrLn ("======== " ++ name) >> -- anonymous bind (no lambda needed)
  putStrLn content
