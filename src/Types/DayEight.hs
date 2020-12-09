module Types.DayEight where

data HandheldOps = Nop { useless :: Int }
                 | Jmp { nextInstruction :: Int }
                 | Acc { additionalValue :: Int }
  deriving (Show, Eq)

instance Read HandheldOps where
  readsPrec _ input =
    let [op, arg] = words input
        offset = if (head arg) == '+'
                    then read (tail arg) :: Int
                    else read arg :: Int
    in case op of
         "jmp" -> [(Jmp offset, "")]
         "acc" -> [(Acc offset, "")]
         _ -> [(Nop offset, "")]
