# Read in YRBSS data for question on physical activity and grades


# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)


# Read in data ------------------------------------------------------------

start_pos <- c(1,  6,  56, 114, 314, 325, 162, 165, 171, 700, 263)
end_pos   <- c(5, 55, 105, 121, 314, 325, 164, 167, 173, 700, 263)
var_names <- c("sitecode","sitename","sitetype","year", "q78", "q89", "sex",
               "grade", "race7", "qwenthungry", "q25")

yrbs <- bind_rows(
  read_fwf(here("input","yrbss","sadc_2019_state_a_m.dat.gz"),
           col_positions = fwf_positions(start = start_pos, end = end_pos,
                                         col_names = var_names),
           col_types = cols(sex = "i", grade="i", race7="i"),
           na = c(".","")),
  read_fwf(here("input","yrbss","sadc_2019_state_n_z.dat.gz"),
           col_positions = fwf_positions(start = start_pos, end = end_pos,
                                         col_names = var_names),
           col_types = cols(sex = "i", grade="i", race7="i"),
           na = c(".","")))



# Clean data --------------------------------------------------------------


yrbs <- yrbs |>
  filter(year==2019) |>
  mutate(physical_act=q78-1,
         grades=case_when(
           q89==1 ~ 4,
           q89==2 ~ 3,
           q89==3 ~ 2,
           q89==4 ~ 1,
           q89==5 ~ 0),
         gender=factor(case_when(
           sex==1 ~ "Female",
           sex==2 ~ "Male")),
         race=factor(race7, levels=c(6,4,3,2,1,5,7),
                     labels=c("White","Latino","Black","Asian","American Indian",
                            "Pacific Islander","Multiple Races")),
         school_grade=factor(grade, levels=1:4, labels=c("9th grade","10th grade",
                                                  "11th grade", "12th grade")),
         hungry=factor(qwenthungry, levels=1:5,
                       labels=c("Never","Rarely","Sometimes","Most of the time",
                                "Always")),
         felt_sad=factor(q25, levels=2:1, labels=c("No","Yes")),
         state=str_split_i(sitename, " \\(", i=1)
  ) |>
  select(state, grades, physical_act, gender, race, school_grade, hungry,
         felt_sad) |>
  #hungry variable only asked in certain states so exclude NA values
  filter(!is.na(hungry) & !is.na(grades))

yrbs <- complete(mice(yrbs, 1))

# Save data ---------------------------------------------------------------

save(yrbs, file=here("output","yrbs.RData"))

