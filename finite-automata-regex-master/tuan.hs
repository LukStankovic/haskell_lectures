state = [1,2,3,4,5]

stateToDot :: Int -> String
stateToDot q |q == (maximum state) = show q ++ "[shape=" ++ "\"circle\"" ++ ",label=\"q" ++ show q ++"\"];"
             |otherwise = show q ++ "[shape=" ++ "\"doublecircle\"" ++ ",label=\"q" ++ show q ++ "\"];"

