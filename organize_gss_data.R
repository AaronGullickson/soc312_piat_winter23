# This script will read in a GSS extract for the PIAT project focusing on
# the relationship between income and happiness


# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)


# Read GSS data -----------------------------------------------------------

gss <- read_fwf(here("input","gss","GSS.dat"),
                col_positions =
                  fwf_positions(
                    start=c(1, 6,11,16,21,26,28,33,37,41,46,51,53,57,62),
                    end=  c(5,10,15,20,25,27,32,36,40,45,50,52,56,61,68),
                    col_names = c("sex_orient","hispanic","racecen1","racecen2",
                                  "racecen3","ballot","age","educ","degree",
                                  "sex","income06","region","happy","year",
                                  "id")),
                col_types = cols(.default = "i"))

# Recode/Clean ------------------------------------------------------------

temp <- gss |>
  mutate(
    race=factor(case_when(
      racecen1==16 | hispanic>1 ~ "Latino",
      racecen2>0 ~ "Multiracial",
      racecen1==1 ~ "White",
      racecen1==2 ~ "Black",
      racecen1==3 ~ "American Indian",
      racecen1>=4 & racecen1<11 ~ "Asian",
      racecen1>=11 & racecen1<15 ~ "Pacific Islander",
      racecen1==15 ~ "Other"), levels=c("White","Latino","Black","Asian",
                                        "American Indian","Pacific Islander",
                                        "Multiracial","Other")),
    age=ifelse(age<18, NA, age),
    happy=ifelse(happy<1, NA, 2-happy),
    gender=factor(case_when(
      sex==1 ~ "Male",
      sex==2 ~ "Female")),
    educ=ifelse(educ<0, NA, educ),
    region=factor(case_when(
      region<3 ~ "Northeast",
      region<5 ~ "Midwest",
      region<8 ~ "South",
      region<10 ~ "West")),


  ) |>
  select(happy, gender, age, race, educ, region, )


# Save data ---------------------------------------------------------------


