# Exploratory Analysis:
# Authors: Carolina Medeiros, Rogerio Barbosa, Flavio Carvalhaes

# 0. Packages and Setup --------------------------------------------------------

library(rio)
library(tidyverse)
library(tidylog)
library(segregation)
library(Hmisc)
library(here)

#rm(list = ls());gc()
options(scipen = 999)

data_wd = "../1_Data/processed/"

# 1. Opening Data --------------------------------------------------------------

d_stacked <- import(here(data_wd, "stacked_census.fst"))
d_aggreg  <- import(here(data_wd, "data_levelfields_by_gender.fst"))