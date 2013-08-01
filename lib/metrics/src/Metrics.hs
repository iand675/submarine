module Metrics where

data SerialMetric = SerialMetric
	{ _serialmetricName :: Text
	, _serialmetricValue :: MetricValue
	, _serialmetricUnit :: Text
	}

data Metric v u = Metric
	{ _metricValue :: v
	, _metricUnit :: u
	}

data MetricValue
	= Qualitative Text
	| Quantitative QuantitativeValue

data QuantitativeValue = Double Double | Int Int

instance Num v => Num (Metric v u) where
	...

totalCoffeeDrank :: [Metric Int CoffeeCupsDrank] -> Metric Int CoffeeCupsDrank
totalCoffeeDrank = sum
