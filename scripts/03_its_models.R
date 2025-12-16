############################################################
# Project: Pediatric TB during COVID-19 in Brazil
# Repo: pediatric-tb-covid-brazil-data
# Purpose: Reproducible analysis (aggregated data + scripts)
# Data: Public, de-identified secondary data (SINAN/SIH/SIM/IBGE)
# Notes: This script assumes data are already aggregated and stored in /data/processed
############################################################

# ---- 0) Setup ----
rm(list = ls()); gc()

# Use here::here() to avoid absolute paths
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# (Optional) enforce project root
# setwd(here::here())

# Packages
pkgs <- c("dplyr", "readr", "stringr", "lubridate", "ggplot2", "broom")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 1) Paths ----
DIR_DATA   <- here("data")
DIR_PROC   <- here("data", "processed")
DIR_OUT_T  <- here("outputs", "tables")
DIR_OUT_F  <- here("outputs", "figures")

dir.create(DIR_OUT_T, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_F, recursive = TRUE, showWarnings = FALSE)

# ---- 2) Input ----
# Example: aggregated quarterly dataset (counts + population by race)
infile <- file.path(DIR_PROC, "tb_0to4_quarterly_by_race.csv")
stopifnot(file.exists(infile))

df <- readr::read_csv(infile, show_col_types = FALSE)

# EXPECTED columns (edit to match your file):
# year, quarter, time_index, race, notif_count, hosp_count, death_count, population
# plus optional: intervention (0/1), time_after (0..)

# ---- 3) Basic checks ----
stopifnot(all(c("time_index","race","population") %in% names(df)))
stopifnot(all(df$population > 0))
