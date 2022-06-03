#### Analysis for chapter 3 ####################################################

library(tidyverse)
library(lubridate)
library(slider)
library(tsibble)
library(feasts)
library(fable)
library(sf)

qs::qload("output/data/data_processed.qsm", nthreads = future::availableCores())
qs::qload("output/data/FREH_model.qsm", nthreads = future::availableCores())
qs::qload("output/data/model_chapter.qsm", nthreads = future::availableCores())
