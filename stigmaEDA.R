# ++++++++ Stigma EDA ++++++++ 

# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
# load('data/datatb1.rdata')
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(haven)

data <- datatb1

#################################################
## Data Processing and Manipulation (Baseline) ##
#################################################
### Create additional "stigma_score" column using sum a1_q30 to a1_q41, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
datatb1 <- datatb1 %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))

# statistical summary of crude sigma_score
summary(datatb1$stigma_score)

# Categorize stigma_score using mean (use mean of 25.1, as we compared with  a similar study)
threshold <- 25.1
datatb1 <- datatb1 %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > threshold ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= threshold ~ "Low",  # Categorize as Low if stigma_score <= 15.4
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


##################################################
## Data Processing and Manipulation (Follow up) ##
##################################################
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

# filtered follow up data set for participants who completed followed up
datatb2_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% 1)

## results from baseline for particiants who follow up completed
baseline_followup_complete <- datatb1[datatb1$a1_record_id %in% datatb2_followup$a1_record_id, ]

summary(baseline_followup_complete$stigma_score)
par(mfrow = c(2, 2)) # set plotting layout 2x2
hist1 <- hist(baseline_followup_complete$stigma_score, main='Completed follow-up', xlab='Stigma Score', labels=TRUE)

# filtered follow up data set for participants who did not follow up (dead/loss/refuse) to get their a1_record_id
# all dnf
datatb2_no_followup <- datatb2 %>%
  filter(a1_status_tb_during_fu %in% c(2, 3, 4))
# refuse dnf
datatb2_refuse_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 3)
# loss dnf
datatb2_loss_to_followup_stigma <- datatb2 %>%
  filter(a1_status_tb_during_fu == 4)

## results from baseline for participants who did not follow up 
baseline_dnf <- datatb1[datatb1$a1_record_id %in% datatb2_no_followup$a1_record_id, ]
baseline_refuse_followup <- datatb1[datatb1$a1_record_id %in% datatb2_refuse_to_followup_stigma$a1_record_id, ]
baseline_loss_followup <- datatb1[datatb1$a1_record_id %in% datatb2_loss_to_followup_stigma$a1_record_id, ]


# see overview of baseline stigma scores for those who did not follow up
baseline_dnf %>% 
  group_by(stigma_score) %>%
  summarise(
    count=n()
  ) %>% print(n=25)

summary(baseline_dnf$stigma_score)
hist2 <- hist(baseline_dnf$stigma_score, main='Did not follow-up', xlab='Stigma Score', labels=TRUE)
summary(baseline_refuse_followup$stigma_score)
hist3 <- hist(baseline_refuse_followup$stigma_score,main='Refuse to follow-up', xlab='Stigma Score', labels=TRUE)
summary(baseline_loss_followup$stigma_score)
hist4 <- hist(baseline_loss_followup$stigma_score, main='Loss to follow-up', xlab='Stigma Score', labels=TRUE)

t.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)] , paired=TRUE)
boxplot(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)], names = c("Baseline", "Follow up"))
hist(datatb1$stigma_score)
hist(datatb2$stigma_score)



####################################################
##  EDA Section 3.1 Stigma Experiences (Baseline) ##
####################################################
# Responses: Strongly Disagree(0) | Disagree(1) | No Opinion(2) | Agree(3) | Strongly Agree(4)

# a1_q30.I feel hurt how others react to knowing I have TB
table(datatb1$a1_q30)
# a1_q31.I lose friends when I share with them that I have TB
table(datatb1$a1_q31)
# a1_q32.I feel alone
table(datatb1$a1_q32)
# a1_q33.I keep a distance from others to avoid spreading TB germs
table(datatb1$a1_q33)
# a1_q34.I am afraid to tell those outside my family that I have TB
table(datatb1$a1_q34)
# a1_q35.I am afraid of going to TB clinics because see me there
table(datatb1$a1_q35)
# a1_q36.I am afraid to I have TB because they I also have HIV/AIDS
table(datatb1$a1_q36)
# a1_q37.I feel guilty because my family has burden caring for me
table(datatb1$a1_q37)
# a1_q38.I choose carefully who I tell about having TB
table(datatb1$a1_q38)
# a1_q39.I feel guilty for getting TB because my smoking, drinking
table(datatb1$a1_q39)
# a1_q40.I am worried about having HIV/AIDS
table(datatb1$a1_q40)
# a1_q41.I am afraid to tell my family that I have TB
table(datatb1$a1_q41)



##############################################################################################
##  EDA Section 3.2 Center for Epidemiology Studies Depression Scale (CES-D-10): (Baseline) ##
##############################################################################################
## questions a1_q42 to a1_q51
## A total score can be varied from 0 to 30. Cutting score:(≥10) = Cutoff score of 10 or higher on CES-D is indicative of depression symptoms
### Create additional "CES-D10" column using sum a1_q42 to a1_q51. Subtract 1 as the survey uses 1-based scale
datatb1 <- datatb1 %>%
  mutate(CES_D_10 = rowSums(across(a1_q42:a1_q51)-1))

# statistical summary of crude CES_D_10
summary(datatb1$CES_D_10)

# Categorize CES_D_10 using 10 (based on study)
threshold <- 10
datatb1 <- datatb1 %>%
  mutate(
    CES_D_10_threshold = case_when(
      CES_D_10 >= threshold ~ "1",  # Categorize as 1 if CES_D_10e >= 10
      CES_D_10 < threshold ~ "0",  # Categorize as 0 if CES_D_10 < 10
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# Visualize distributions of CES_D_10 at baseline
ggplot(datatb1, aes(x = CES_D_10)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of CES_D_10", x = "CES_D_10", y = "Frequency")

# Visualize distributions of CES_D_10_threshold at baseline using bar plot
ggplot(datatb1, aes(x = CES_D_10_threshold)) + 
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Distribution of CES_D_10", x = "CES_D_10 Threshold", y = "Frequency") +
  theme_minimal()


