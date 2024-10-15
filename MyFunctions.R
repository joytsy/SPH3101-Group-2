####################################################################
## Instructions to use functions in this script: 
## 1. Load the function
## ```source("path/to/MyFunctions.R")```
## 2. Use the function to process your data
## ```data_processed <- process_function_name(datatb1)```
####################################################################
source('load_data.r')
process_datatb1_function <- function(raw_data) {
  # Function Code:
  processed_data <- raw_data
  # Remove observations where individuals are under 18
  processed_data <- processed_data %>% 
    filter(a1_q3 >= 18)
  # Convert categorical variables to factors
  processed_data[,"a1_q1"] <- as.factor(processed_data[,"a1_q1"]) # sex
  processed_data[,"a1_q5"] <- as.factor(processed_data[,"a1_q5"]) # education
  processed_data[,"a1_q16"] <- as.factor(processed_data[,"a1_q16"]) # smoked previously
  processed_data[,"a1_q18"] <- as.factor(processed_data[,"a1_q18"]) # drink alcohol
  # Perform row sum across stigma scores, and create additional "stigma_score" column, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
  processed_data <- processed_data %>%
    mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))
  # Create new column to categorize individuals experiencing high and low stigma using baseline mean of 15.4(string version) 
  processed_data <- processed_data %>%
    mutate(
      stigma_threshold = case_when(
        stigma_score > 15.4 ~ "High",  # High if stigma_score > 15.4
        stigma_score <= 15.4 ~ "Low",  # Low if stigma_score <= 15.4
        TRUE ~ NA_character_  # Handle any unexpected cases
      )
    )
  # Create new column to categorize individuals experiencing high and low stigma using baseline mean of 15.4(numerical version)
  processed_data <- processed_data %>%
    mutate(
      stigma_thresNumber = case_when(
        stigma_score > 15.4 ~ 1,  # 1 if stigma_score > 15.4
        stigma_score <= 15.4 ~ 0,  # 0 if stigma_score <= 15.4
        TRUE ~ NA_real_  # Handle any unexpected cases
      )
    )
  # Categorize education levels into 'primary and below' and 'above primary'
  processed_data <- processed_data %>%
    mutate(
      abovePrimary = case_when(
        a1_q5 %in% c(1, 5) ~ "No",  # No formal schooling or primary school only
        a1_q5 %in% c(2, 3, 4) ~ "Yes",  # Above primary school
        TRUE ~ NA_character_  # Handle any unexpected cases
      )
    )
  # Categorize alcohol consumption into drinker or non-drinker
  processed_data <- processed_data %>%
    mutate(
      alcoholConsumer = case_when(
        a1_q18 %in% c(5) ~ "Non-Drinker",  # Non-drinker
        a1_q5 %in% c(1, 2, 3, 4) ~ "Drinker",  # Drinker
        TRUE ~ NA_character_  # Handle any unexpected cases
      )
    )
  # Convert treatment dates to Date format and calculate treatment duration
  processed_data <- processed_data %>%
    mutate(Start.treatment.date = as.Date(Start.treatment.date, format = "%Y-%m-%d"),
           Finish.treatment.date = as.Date(Finish.treatment.date, format = "%Y-%m-%d"),
           treatmentDuration = Finish.treatment.date - Start.treatment.date, .after = Finish.treatment.date)
  return(processed_data)
}



##############################################################
## run process_datatb1_function before running this function
##############################################################
process_datatb2_function <- function(raw_data) {
  # Function Code:
  processed_data <- raw_data
  # Remove observations where individuals are under 18
  processed_data <- processed_data %>% 
    filter(a1_record_id %in% datatb1$a1_record_id)
  # Perform row sum across stigma scores, and create additional "stigma_score" column, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
  processed_data <- processed_data %>%
    mutate(stigma_score = rowSums(across(a1_q7_fu:a1_q18_fu)))
  # Create new column to categorize individuals experiencing high and low stigma using baseline mean of 15.4(string version) 
  processed_data <- processed_data %>%
    mutate(
      stigma_threshold = case_when(
        stigma_score > 15.4 ~ "High",  # High if stigma_score > 15.4
        stigma_score <= 15.4 ~ "Low",  # Low if stigma_score <= 15.4
        TRUE ~ NA_character_  # Handle any unexpected cases
      )
    )
  # Create new column to categorize individuals experiencing high and low stigma using baseline mean of 15.4(numerical version)
  processed_data <- processed_data %>%
    mutate(
      stigma_thresNumber = case_when(
        stigma_score > 15.4 ~ 1,  # 1 if stigma_score > 15.4
        stigma_score <= 15.4 ~ 0,  # 0 if stigma_score <= 15.4
        TRUE ~ NA_real_  # Handle any unexpected cases
      )
    )
  return(processed_data)
}
