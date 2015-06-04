#!/usr/bin/env runhaskell
--
-- to make a script
--
-- (1) put '#!/usr/bin/env runhaskell' at top of file
-- (2) create a module Main
-- (3) create a main function
-- (4) ensure your hs file (script) is chmod + x
--
-- then
-- $ Passwd.hs
--
-- to run your file
module Main where

main :: IO ()
main =
  readFile "/etc/passwd" >>= putStrLn
