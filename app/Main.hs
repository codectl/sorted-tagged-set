module Main where

import SortedTaggedSet

ts1 = insertTag "todo" 'a' $
      insertTag "ok" 'c' $
      insertTag "urgent" 'c' $
      insertTag "urgent" 'b' $
      insertTag "ok" 'b' $
      insertSet 'b' $
      insertSet 'c' $
      singleton 'a'

ts2 = insertTag "another todo" 'a' $
      insertTag "ok" 'c' $
      insertTag "nok" 'c' $
      insertTag "urgent" 'c' $
      insertTag "ok" 'd' $
      insertSet 'd' $
      insertSet 'c' $
      insertSet 'a' empty

main = do putStrLn $ show ts1
          putStrLn $ show ts2
          putStrLn $ show (empty::SortedTaggedSet Int)
          putStrLn $ show $ nullSet ts1
          putStrLn $ show $ belongs 'x' ts1
          putStrLn $ show $ singleton 12
          putStrLn $ show $ insertSet 12 (singleton 23)
          putStrLn $ show $ removeSet 'a' ts1
          putStrLn $ show $ insertTag "todo" 'a' ts1
          putStrLn $ show $ insertTag "todo" 'x' ts1
          putStrLn $ show $ merge ts1 ts2