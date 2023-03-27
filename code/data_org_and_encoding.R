# Set local working directory
setwd("~/Desktop/epistemic_analytics/mamta_elsevier")

# Import necessary libraries
library(tidyverse)

# Read in relevant datasets
lucas_rakshya <- read_csv("./datasets/lucas_rakshya.csv")
lucas_ryleigh <- read_csv("./datasets/lucas_ryleigh.csv")
quan_rakshya <- read_csv("./datasets/quan_rakshya.csv")
quan_ryleigh <- read_csv("./datasets/quan_ryleigh.csv")
regina_rakshya <- read_csv("./datasets/regina_rakshya.csv") 
regina_ryleigh <- read_csv("./datasets/regina_ryleigh.csv")

# Deselect additional columns (all NA values anyways), drop unnecessary cases of regina_rakshya (random NA rows)
# Note, used which(!complete.cases("df_name")) for each of these to make sure no extraneous rows has NA values, df_name
# being each one of the six dataframes
lucas_rakshya <- lucas_rakshya %>%
  select(-c(1))
lucas_ryleigh <- lucas_ryleigh %>%
  select(-c(1)) 
quan_rakshya <- quan_rakshya %>%
  select(-c(1)) 
quan_ryleigh <- quan_ryleigh %>%
  select(-c(1)) 
regina_rakshya <- regina_rakshya %>%
  select(-c(1, 21)) %>%
  drop_na()
regina_ryleigh <- regina_ryleigh %>%
  select(-c(1)) 

# Bind the six dataframes together
lucas <- rbind(lucas_rakshya, lucas_ryleigh)
quan <- rbind(quan_rakshya, quan_ryleigh)
regina <- rbind(regina_rakshya, regina_ryleigh)
temp <- rbind(lucas, quan)
df <- rbind(temp, regina) 

# Encode new columns
df <- df %>%
  mutate(RC = case_when((`Interaction Type` == "Exam Action") ~ 1, (`Interaction Type` != "Exam Action") ~ 0), .before = CMC) %>%
  mutate(AC = case_when((`SH Feature` == "Objective Data Collection" & `Correct Attempt` == "TRUE") ~ 1, 
                        (`SH Feature` != "Objective Data Collection" | `Correct Attempt` != "TRUE") ~ 0), .after = RC) %>%
  mutate(GS = case_when((`SH Feature` == "Planning") ~ 1, (`SH Feature` != "Planning") ~ 0), .after = AC) %>%
  mutate(EO = case_when((`SH Feature` == "Intervention & Evaluation Rationale") ~ 1, 
                        (`SH Feature` != "Intervention & Evaluation Rationale") ~ 0), .after = GS) %>%
  mutate(DPH = case_when((`SH Feature` == "Nursing Diagnosis") ~ 1, (`SH Feature` != "Nursing Diagnosis") ~ 0), .after = SA) %>%
  relocate(CMC, .after = DPH)

# Write the outputted dataframe to a csv in the "datasets" folder
write.csv(df, "./datasets/data_org_and_encoded.csv", row.names = FALSE)



