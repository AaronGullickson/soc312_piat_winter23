# This script will read in a GSS extract for the PIAT project focusing on
# the relationship between income and happiness


# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)

# Read GSS data -----------------------------------------------------------

gss <- read_fwf(here("input","gss","GSS.dat"),
                col_positions =
                  fwf_positions(
                    start=c(1,5, 9,14,19,21,25,29,33,35,39,41,46,51),
                    end=  c(4,8,13,18,20,24,28,32,34,38,40,45,50,55),
                    col_names = c("hispanic","racecen1","racecen2","racecen3",
                                  "ballot","marital","age","educ","sex",
                                  "income06","region","happy","year","id")),
                col_types = cols(.default = "i"))

# Recode/Clean ------------------------------------------------------------

# wow, apparently GSS is still top-coding income at $25K, like WTF?
# from 2006 to 2014, they also asked it at higher intervals, but then apparently
# went back to original after 2014? I can find no later bracketing on GSS
# Data Explorer

gss <- gss |>
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
    happiness=ifelse(happy<1, NA, 2-happy),
    gender=factor(case_when(
      sex==1 ~ "Male",
      sex==2 ~ "Female")),
    educ=ifelse(educ<0, NA, educ),
    region=factor(case_when(
      region<3 ~ "Northeast",
      region<5 ~ "Midwest",
      region<8 ~ "South",
      region<10 ~ "West")),
    marstat=factor(case_when(
      marital == 1 ~ "Married",
      marital == 2 ~ "Widowed",
      marital == 3 ~ "Divorced",
      marital == 4 ~ "Separated",
      marital == 5 ~ "Never Married"),
      levels=c("Never Married","Married",
               "Divorced","Separated","Widowed")),
    fam_income=factor(case_when( # do income in rough deciles
      income06<0 ~ NA_character_,
      income06<9 ~ "Less than 10K",
      income06<12 ~ "10K-17.5K",
      income06<15 ~ "17.5K-25K",
      income06<17 ~ "25K-35K",
      income06<19 ~ "35K-50K",
      income06<20 ~ "50K-60K",
      income06<21 ~ "60K-75K",
      income06<22 ~ "75K-90K",
      income06<24 ~ "90K-130K",
      income06<26 ~ "Greater than 130K"),
      levels=c("Less than 10K","10K-17.5K","17.5K-25K","25K-35K","35K-50K",
               "50K-60K","60K-75K","75K-90K","90K-130K","Greater than 130K"))
  ) |>
  select(happiness, fam_income, gender, age, race, educ, marstat, region,
         year) |>
  filter(!is.na(happiness))

# now lets use mice to impute missing values
gss <- complete(mice(gss, 1))

# Save data ---------------------------------------------------------------

save(gss, file=here("output","gss.RData"))

