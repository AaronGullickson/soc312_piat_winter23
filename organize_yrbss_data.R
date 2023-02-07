# Read in YRBSS data for question on physical activity and grades


# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)


# Read in data ------------------------------------------------------------

start_pos <- c(1, 6, 56,114, 314, 325)
end_pos   <- c(5,55,105,121, 314, 325)
var_names <- c("sitecode","sitename","sitetype","year", "q78", "q89")

yrbs <- bind_rows(
  read_fwf(here("input","yrbss","sadc_2019_state_a_m.dat.gz"),
           col_positions = fwf_positions(start = start_pos, end = end_pos,
                                         col_names = var_names)),
  read_fwf(here("input","yrbss","sadc_2019_state_n_z.dat.gz"),
           col_positions = fwf_positions(start = start_pos, end = end_pos,
                                         col_names = var_names)))



# Clean data --------------------------------------------------------------

temp <- yrbs |>
  filter(year==2019)
