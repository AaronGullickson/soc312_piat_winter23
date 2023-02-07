# This script will read in the CES data for the PIAT research question on
# political ideology and age


# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)

# Read in data ------------------------------------------------------------

ces <- read_csv(here("input","ces20","CES20_Common_OUTPUT_vv.csv.gz"))

# its the whole enchilada so lets trim it down and recode
ces <- ces |>
  mutate(conservative=ifelse(CC20_340a==8, NA, CC20_340a),
         age=2020-birthyr,
         age_group=factor(case_when(
           age < 25 ~ "18-24",
           age < 35 ~ "25-34",
           age < 45 ~ "35-44",
           age < 55 ~ "45-54",
           age < 65 ~ "55-64",
           age < 75 ~ "65-74",
           age < 85 ~ "75-85",
           age < 100 ~ "85+")),
         gender=factor(case_when(
           gender == 1 ~ "Male",
           gender == 2 ~ "Female")),
         degree=factor(case_when(
           educ == 1 ~ "No HS Diploma",
           educ == 2 ~ "HS Diploma",
           educ == 3 | educ == 4 ~ "Some College",
           educ == 5 ~ "4-year College Degree",
           educ == 6 ~ "Graduate Degree"), levels=c("No HS Diploma",
                                                    "HS Diploma",
                                                    "Some College",
                                                    "4-year College Degree",
                                                    "Graduate Degree")),
         race=factor(case_when(
           hispanic==1 | race==3 ~ "Latino",
           race==1 ~ "White",
           race==2 ~ "Black",
           race==4 ~ "Asian",
           race==5 ~ "American Indian",
           race==7 ~ "Multiracial",
           race==6 | race==8 ~ "Other"),
           levels=c("White","Latino","Black","Asian","American Indian",
                    "Multiracial","Other")),
         region=factor(case_when(
           region==1 ~ "Northeast",
           region==2 ~ "Midwest",
           region==3 ~ "South",
           region==4 ~ "West"), levels=c("Northeast","Midwest","South","West")),
         social_media=factor(case_when(
           CC20_300_1==1 ~ "Used social media in last 24 hours",
           CC20_300_1==2 ~ "Did not use social media in the last 24 hours"),
           levels=c("Did not use social media in the last 24 hours",
                    "Used social media in last 24 hours")),
         watch_fox=!is.na(CC20_300b_5) & CC20_300b_5==1,
         watch_msnbc=!is.na(CC20_300b_6) & CC20_300b_6==1
  ) |>
  select(conservative, age, gender, degree, race, region, social_media,
         watch_fox, watch_msnbc) |>
  filter(!is.na(conservative))


# Save data ---------------------------------------------------------------

save(ces, file=here("output","ces.RData"))
