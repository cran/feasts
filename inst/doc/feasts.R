## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 7
)

## ----setup, message=FALSE-----------------------------------------------------
library(feasts)
library(tsibble)
library(dplyr)

## ----data---------------------------------------------------------------------
tourism_melb <- tourism %>%
  filter(Region == "Melbourne")
tourism_melb %>%
  group_by(Purpose) %>%
  slice(1)

## ----plot---------------------------------------------------------------------
tourism_melb %>%
  autoplot(Trips)

## ----season-plot--------------------------------------------------------------
tourism_melb %>%
  gg_season(Trips)

## ----subseries-plot-----------------------------------------------------------
tourism_melb %>%
  gg_subseries(Trips)

## ----acf----------------------------------------------------------------------
tourism_melb %>%
  ACF(Trips)

## ----acf-plot-----------------------------------------------------------------
tourism_melb %>%
  ACF(Trips) %>%
  autoplot()

## ----stl----------------------------------------------------------------------
tourism_melb %>%
  model(STL(Trips ~ season(window = "periodic"))) %>% 
  components()

## ----stl-plot-----------------------------------------------------------------
tourism_melb %>%
  model(STL(Trips ~ season(window = 9))) %>%
  components() %>% 
  autoplot()

## ----features-----------------------------------------------------------------
tourism_melb_features <- tourism_melb %>%
  features(Trips, feature_set(tags = "stl"))
tourism_melb_features

## ----featutes-plot------------------------------------------------------------
library(ggplot2)
tourism_melb_features %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, colour = Purpose)) +
  geom_point() +
  coord_equal() +
  lims(x = c(0,1), y = c(0,1))

## ----features-all-plot--------------------------------------------------------
tourism_features <- tourism %>%
  features(Trips, feat_stl)

ggplot(mapping = aes(x = trend_strength, y = seasonal_strength_year, colour = Purpose)) +
  geom_point(data = tourism_features, alpha = 0.3) +
  geom_point(data = tourism_melb_features, size = 2) +
  coord_equal() +
  facet_wrap(vars(Purpose)) +
  lims(x = c(0,1), y = c(0,1))

