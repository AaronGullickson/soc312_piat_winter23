

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)

# Read Data ---------------------------------------------------------------

anes <- read_csv(here("input","anes2020",
                      "anes_timeseries_2020_csv_20220210.csv"))

# Clean Data --------------------------------------------------------------

anes <- anes |>
  mutate(stereo_black_violent=ifelse(V202522<0, NA, V202522),
         stereo_black_lazy=ifelse(V202516<0, NA, V202516),
         stereo_black=stereo_black_violent+stereo_black_lazy,
         use_tv=ifelse(V201629a<0, NA, V201629a==1),
         use_newspaper=ifelse(V201629b<0, NA, V201629b==1),
         use_internet=ifelse(V201629c<0, NA, V201629c==1),
         use_radio=ifelse(V201629d<0, NA, V201629d==1),
         age=ifelse(V201507x<0, NA, V201507x),
         race=factor(V201549x, levels=1:6, labels=c("White","Black","Latino",
                                                    "AAPI","Native American",
                                                    "Multiple Races")),
         educ=factor(V201511x, levels=1:5,
                     labels=c("No HS Diploma","HS Diploma", "Some College",
                              "Bachelor's Degree","Graduate Degree")),
         foreign_born=V201554!=1) |>
  select(stereo_black,use_tv, use_newspaper, use_internet, use_radio, age, race,
         educ, foreign_born) |>
  filter(!is.na(stereo_black))

anes <- complete(mice(anes, 1))

# Save data ---------------------------------------------------------------

save(anes, file=here("output","anes.RData"))

