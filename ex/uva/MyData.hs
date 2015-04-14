module MyData

(MetricUnit(..),
  ImperialUnit(..),
  Measurement(..),
  convert)

where

data MetricUnit = Meter | Liter | KiloGram deriving (Show, Eq)

symbol :: MetricUnit -> String
--symbol Meter = "m"
--symbol Liter = "L"
--symbol KiloGram = "kg"
symbol unit
  | unit == Meter = "m" 
  | unit == Liter = "L" 
  | unit == KiloGram = "kg"
  | otherwise = error "invalid unit"

data ImperialUnit = Yard | Gallon | Pound deriving (Show, Eq)

--symbol :: ImperialUnit -> String
--symbol Yard = "yd"
--symbol Gallon = "gal"
--symbol Pound = "lb"


data Measurement = MetricMeasurement Double MetricUnit |
	ImperialMeasurement Double ImperialUnit deriving (Show)

convert :: Measurement -> Measurement

convert (MetricMeasurement x u)
  | u == Meter = ImperialMeasurement (1.0936*x) Yard
  | u == Liter = ImperialMeasurement (0.2642*x) Gallon
  | u == KiloGram = ImperialMeasurement (2.2046*x) Pound

convert (ImperialMeasurement x u)
  | u == Yard = MetricMeasurement (x/1.0936) Meter
  | u == Gallon = MetricMeasurement (x/0.2642) Liter
  | u == Pound = MetricMeasurement (x/2.2046) KiloGram
