main = do putStr "Your name: "
          a <- getChar
          putStr("\nhello " ++[a])
          