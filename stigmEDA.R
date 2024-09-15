# ++++++++ Stigma EDA ++++++++ 

# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
# load('data/datatb1.rdata')
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(haven)

# -------- Data Processing and Manipulation (Baseline) --------
### Create additional "stigma_score" column using sum a1_q30 to a1_q41, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
datatb1 <- datatb1 %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))

# statistical summary of crude sigma_score
summary(datatb1$stigma_score)

# Categorize stigma_score using mean
datatb1 <- datatb1 %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > 15.4 ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= 15.4 ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# Visualize distributions of stigma_score at baseline
ggplot(datatb1, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_threshold at baseline using bar plot
ggplot(datatb1, aes(x = stigma_threshold)) + 
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores", x = "Stigma Threshold", y = "Frequency") +
  theme_minimal()

# -------- Data Processing and Manipulation (Follow up) --------
datatb2 <- datatb2 %>%
  mutate(stigma_score = rowSums(across(a1_q7_fu:a1_q18_fu)))

summary(datatb2$stigma_score)

# Visualize distributions of stigma scores at follow up
ggplot(datatb2, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores", x = "Stigma Score", y = "Frequency")

# -------- Data Processing and Manipulation --------
# Convert the labelled variable to numeric or factor
datatb2 <- datatb2 %>%
  mutate(a1_status_tb_during_fu = as.numeric(a1_status_tb_during_fu))

# Brief summary of count of reasons for individuals who did not follow up 
datatb2 %>% 
  mutate(a1_status_tb_during_fu = recode(a1_status_tb_during_fu,
                                               `1` = "Alive",
                                               `2` = "Dead",
                                               `3` = "Refuse to follow-up",   
                                               `4` = "Loss to follow-up")) %>%
  group_by(a1_status_tb_during_fu) %>%
  summarise(
    count=n()
  )

# filtered follow up data set for participants who did not follow up
no_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% c(2, 3, 4))

refuse_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 3)

loss_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 4)

# results from baseline for participants who did not follow up 
dnf_baseline <- left_join(no_followup, datatb1, by="a1_record_id")

# see overview of baseline stigma scores for those who did not follow up
dnf_baseline %>% 
  filter(is.na(dnf_baseline$a1_q9_fu)) %>% 
  group_by(stigma_score.y) %>%
  summarise(
    count=n()
  ) %>% print(n=25)

summary(dnf_baseline$stigma_score.y)
