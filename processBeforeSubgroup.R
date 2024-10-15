# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
load('data/datatb1.rdata')
load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
# library(reshape2)

#################################################
## Data Processing and Manipulation (Baseline) ##
#################################################
# remove observations where individuals < 18 yo
datatb1 <- datatb1 %>% 
  filter(a1_q3>=18)

### Create additional "stigma_score" column using sum a1_q30 to a1_q41, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
datatb1 <- datatb1 %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))

# Categorize stigma_score using mean (use baseline mean of 15.4)
threshold <- 15.4 # mean of baseline stigma_scores
datatb1 <- datatb1 %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > threshold ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= threshold ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

##################################################
## Data Processing and Manipulation (Follow up) ##
##################################################
# remove observations where individuals < 18 yo (Assuming we have filtered them in datatb1 already)
datatb2 <- datatb2 %>%
  filter(a1_record_id %in% datatb1$a1_record_id)

# Create "stigma_score" column for Follow-up
datatb2 <- datatb2 %>%
  mutate(stigma_score = rowSums(across(a1_q7_fu:a1_q18_fu)))

# Categorize stigma_score using mean (use baseline mean of 15.4)
datatb2 <- datatb2 %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > threshold ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= threshold ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )
####################################################################
# Final Filtered Datasets for all 621 Participants who followed up #
####################################################################
# Followup Dataset for those 621 who completed followup
datatb2_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% 1)

## Baseline Dataset for the 621 participants who completed followup
baseline_followup_complete <- datatb1[datatb1$a1_record_id %in% datatb2_followup$a1_record_id, ]
