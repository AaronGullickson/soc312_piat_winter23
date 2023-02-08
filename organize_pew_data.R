

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(mice)

# Read data ---------------------------------------------------------------

pew <- read_csv(here("input","pew_core_trends",
                     "Jan 25-Feb 8, 2021 - Core Trends Survey - CSV.csv"))



# Clean data --------------------------------------------------------------

pew <- pew |>
  mutate(tiktok=as.numeric(ifelse(web1j>2, NA, web1j==1)),
         age=ifelse(age>=99, NA, age),
         gender=factor(case_when(
           gender==1 ~ "Male",
           gender==2 ~ "Female",
           gender==3 ~ "Non-binary")),
         degree=factor(case_when(
           educ2<3 ~ "No HS Diploma",
           educ2<4 ~ "HS Diploma",
           educ2==5 ~ "Two-year degree",
           educ2<8 ~ "Four-year degree",
           educ2==8 ~ "Graduate/Professional degree"),
           levels=c("No HS Diploma","HS Diploma","Two-year degree",
                    "Four-year degree","Graduate/Professional degree")),
         employ_status=factor(emplnw, levels=1:4,
                              labels=c("Employed full time",
                                       "Employed part time", "Retired",
                                       "Not employed for pay"))
         ) |>
  select(tiktok, age, gender, degree, employ_status)


pew <- complete(mice(pew, 1))

# Save data ---------------------------------------------------------------

save(pew, file=here("output","pew.RData"))
