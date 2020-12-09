module Types.DayThree where

data GeoType = Path | Tree
  deriving (Show, Eq)

newtype GeoLine = GeoLine { line :: [GeoType] }

instance Read GeoType where
  readsPrec _ input =
    let (x:rest) = input
    in
      case x of
        '.' -> [(Path, rest)]
        '#' -> [(Tree, rest)]
        _   -> []

instance Read GeoLine where
  readsPrec _ input =
    let currentGeoLine = map (\x -> read [x] :: GeoType) input
    in [(GeoLine (cycle currentGeoLine), "")]
