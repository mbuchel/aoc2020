module Types.DayFive where

data AirPlaneSeatType = Front | Back | LeftSeat | RightSeat
  deriving (Show, Eq)

newtype Seating = Seating  { seat :: [AirPlaneSeatType] }
  deriving Show

instance Read AirPlaneSeatType where
  readsPrec _ input =
    let (x:rest) = input
    in
      case x of
        'F' -> [(Front, rest)]
        'B' -> [(Back, rest)]
        'L' -> [(LeftSeat, rest)]
        'R' -> [(RightSeat, rest)]
        _   -> []

instance Read Seating where
  readsPrec _ input =
    let seating = map (\x -> read [x] :: AirPlaneSeatType) input
    in [(Seating (seating), "")]
