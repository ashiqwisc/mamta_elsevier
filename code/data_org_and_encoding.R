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
# Note, used which(!complete.cases("df_name")) for each of these to make sure no rows in the google sheets have NA values, 
# df_name being each one of the six dataframes
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

# Parse timestamps into lubridate objects
df <- df %>%
  # Subset everything before the occurrence of a character; depending on the timestamp, this could be either "A" (AM) or "P" PM
  mutate(end_location = str_locate(`Time Stamp`, "[[:upper:]]")) %>%
  rowwise() %>%
  mutate(`Time Stamp` = list(str_sub(`Time Stamp`, start = 1, end = end_location-2))) %>%
  select(-end_location) %>%
  mutate(`Time Stamp` = `Time Stamp`[[1]]) %>%
  # Parse timestamp
  mutate(`Time Stamp` = mdy_hm(`Time Stamp`)) 

# Pivot wider the modality and interaction types; these will serve as codes as well; make these newly pivoted columns binary values
df <- df %>%
  # This gets rid of duplicates. Ask Yeyu if she'd like duplicates; if so, use this instead
  # pivot_wider(names_from = Modality, values_from = 'Modality', values_fill = 0, values_fn = function(x) 1)
  mutate(Dialog = case_when(Modality == "Dialog" ~ 1, Modality != "Dialog" ~ 0)) %>%
  mutate(Click = case_when(Modality == "Click" ~ 1, Modality != "Click" ~ 0)) %>%
  mutate(Documentation = case_when(Modality == "Documentation" ~ 1, Modality != "Documentation" ~ 0)) %>% 
  # Likewise, gets rid of duplicates
  # pivot_wider(names_from = `Interaction Type`, values_from = 'Interaction Type', values_fill = 0, values_fn = function(x) 1) 
  mutate(Greet = case_when(`Interaction Type` == "Greet" ~ 1, `Interaction Type` != "Greet" ~ 0)) %>%
  mutate(Response = case_when(`Interaction Type` == "Response" ~ 1, `Interaction Type` != "Response" ~ 0)) %>%
  mutate(Question = case_when(`Interaction Type` == "Question" ~ 1, `Interaction Type` != "Question" ~ 0)) %>%
  mutate(Clarification = case_when(`Interaction Type` == "Clarification" ~ 1, `Interaction Type` != "Clarification" ~ 0)) %>%
  mutate(Feedback = case_when(`Interaction Type` == "Feedback" ~ 1, `Interaction Type` != "Feedback" ~ 0)) %>%
  mutate(Statement = case_when(`Interaction Type` == "Statement" ~ 1, `Interaction Type` != "Statement" ~ 0)) %>%
  mutate(`Exam Action` = case_when(`Interaction Type` == "Exam Action" ~ 1, `Interaction Type` != "Exam Action" ~ 0)) %>%
  mutate(Answer = case_when(`Interaction Type` == "Answer" ~ 1, `Interaction Type` != "Answer" ~ 0)) %>%
  mutate(Prompt = case_when(`Interaction Type` == "Prompt" ~ 1, `Interaction Type` != "Prompt" ~ 0)) %>%
  mutate(Empathize = case_when(`Interaction Type` == "Empathize" ~ 1, `Interaction Type` != "Empathize" ~ 0)) %>%
  mutate(Educate = case_when(`Interaction Type` == "Educate" ~ 1, `Interaction Type` != "Educate" ~ 0)) 
  
# Write the outputted dataframe to a csv in the "datasets" folder
write.csv(df, "./datasets/data_org_and_encoded.csv", row.names = FALSE)



