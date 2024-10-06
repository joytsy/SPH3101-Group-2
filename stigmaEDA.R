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
# remove observations where individuals < 18 yo
datatb1 <- datatb1 %>% 
  filter(a1_q3>=18)

### Create additional "stigma_score" column using sum a1_q30 to a1_q41, note: max stigma_score is 12*4=48 with 48 being greater stigma experience
datatb1 <- datatb1 %>%
  mutate(stigma_score = rowSums(across(a1_q30:a1_q41)))

# statistical summary of crude sigma_score
summary(datatb1$stigma_score)

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

# Visualize distributions of stigma_score at baseline
ggplot(datatb1, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_threshold at baseline using bar plot
ggplot(datatb1, aes(x = stigma_threshold)) + 
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Baseline)", x = "Stigma Threshold", y = "Frequency") +
  theme_minimal()


##################################################
## Data Processing and Manipulation (Follow up) ##
##################################################
# remove observations where individuals < 18 yo
datatb2 <- datatb2 %>%
  filter(a1_record_id %in% datatb1$a1_record_id)

datatb2 <- datatb2 %>%
  mutate(stigma_score = rowSums(across(a1_q7_fu:a1_q18_fu)))

summary(datatb2$stigma_score)

# Categorize stigma_score using mean (use baseline mean of 15.4)
datatb2 <- datatb2 %>%
  mutate(
    stigma_threshold = case_when(
      stigma_score > threshold ~ "High",  # Categorize as High if stigma_score > 15.4
      stigma_score <= threshold ~ "Low",  # Categorize as Low if stigma_score <= 15.4
      TRUE ~ NA_character_  # Handle any unexpected cases
    )
  )

# Visualize distributions of stigma scores at follow up
ggplot(datatb2, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Follow-Up)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_threshold at follow up using bar plot
ggplot(datatb2, aes(x = stigma_threshold)) + 
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Follow-Up)", x = "Stigma Threshold", y = "Frequency") +
  theme_minimal()

# -------- Data Processing and Manipulation --------
# Convert the labelled variable for status during follow-up to numeric
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

## results from baseline for participants who completed follow-up 
baseline_followup_complete <- datatb1[datatb1$a1_record_id %in% datatb2_followup$a1_record_id, ]

summary(baseline_followup_complete$stigma_score)
par(mfrow = c(2, 2)) # set plotting layout 2x2
hist1 <- hist(baseline_followup_complete$stigma_score, main='Completed follow-up', xlab='Stigma Score', labels=TRUE)

# filtered follow-up dataset for participants who did not follow up (dead/loss/refuse) to get their a1_record_id
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


# Visualize distributions of stigma_score at baseline for the 621 individuals who followed up
ggplot(baseline_followup_complete, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Baseline)", x = "Stigma Score", y = "Frequency")

# Visualize distributions of stigma_score at follow up for the 621 individuals who followed up
ggplot(datatb2_followup, aes(x = stigma_score)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores (Follow-Up)", x = "Stigma Score", y = "Frequency")


# Shapiro-Wilk test to test for normality in both baseline and follow up cohorts of 621 individuals
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)])
# follow-up: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb2$stigma_score[!is.na(datatb2$stigma_score)])

# Since not normally distributed, use wilcoxon to compare the sample of 621 at different time frames
wilcox.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)] , paired=TRUE)
boxplot(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)], names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Follow Up Cohort at Baseline and Follow-Up")

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


#############################################################################
## Subgroup Analysis (Case status; Smoking status; Cough) on stigma_scores ##
#############################################################################

########################### Obtain subgroup datasets from datatb1 and datatb2 ########################### 

##############  case status (only recorded in baseline)
# create new column & copy over "case_status" for those who followed-up in datatb2_followup from datatb1 (exclude died/lost_follow_up/other)
datatb2_followup <- datatb2_followup %>%
  mutate(case_status_copy = datatb1$case_status[datatb1$a1_record_id %in% datatb2_followup$a1_record_id])
# get datasets
datatb2_followup_case_completed <- datatb2_followup %>%
  filter(datatb2_followup$case_status_copy == "completed")
datatb2_followup_case_cured <- datatb2_followup %>%
  filter(datatb2_followup$case_status_copy == "cured")
datatb1_followup_case_completed <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_case_completed$a1_record_id)
datatb1_followup_case_cured <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_case_cured$a1_record_id)

# Shapiro-Wilk test to test for normality
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1_followup_case_completed$stigma_score)
shapiro.test(datatb1_followup_case_cured$stigma_score)
shapiro.test(datatb2_followup_case_completed$stigma_score)
shapiro.test(datatb2_followup_case_cured$stigma_score)
# result: all stigma_scores are not normally distributed -> perform wilcox test

##############  smoking status(only recorded in baseline)
# create new column & copy over smoking_status "a1_q16" for those who followed-up in datatb2_followup from datatb1
datatb2_followup <- datatb2_followup %>%
  mutate(smoke_status_copy = datatb1$a1_q16[datatb1$a1_record_id %in% datatb2_followup$a1_record_id])
# get datasets
datatb2_followup_smoker <- datatb2_followup %>%
  filter(datatb2_followup$smoke_status_copy == 1)
datatb2_followup_nonsmoker <- datatb2_followup %>%
  filter(datatb2_followup$smoke_status_copy == 2)
datatb1_followup_smoker <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_smoker$a1_record_id)
datatb1_followup_nonsmoker <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_nonsmoker$a1_record_id)

# Shapiro-Wilk test to test for normality
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1_followup_smoker$stigma_score)
shapiro.test(datatb1_followup_nonsmoker$stigma_score)
shapiro.test(datatb2_followup_smoker$stigma_score)
shapiro.test(datatb2_followup_nonsmoker$stigma_score)
# result: all stigma_scores are not normally distributed -> perform wilcox test

##############  cough symptom
# create new column & copy over cough "a1_q28___1" for those who followed-up in datatb2_followup from datatb1
datatb2_followup <- datatb2_followup %>%
  mutate(cough_status_copy = datatb1$a1_q28___1[datatb1$a1_record_id %in% datatb2_followup$a1_record_id])
# get datasets
datatb2_followup_cough <- datatb2_followup %>%
  filter(datatb2_followup$cough_status_copy == 1)
datatb2_followup_nocough <- datatb2_followup %>%
  filter(datatb2_followup$cough_status_copy == 0)
datatb1_followup_cough <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_cough$a1_record_id)
datatb1_followup_nocough <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_nocough$a1_record_id)

# Shapiro-Wilk test to test for normality
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1_followup_cough$stigma_score)
shapiro.test(datatb1_followup_nocough$stigma_score)
shapiro.test(datatb2_followup_cough$stigma_score)
shapiro.test(datatb2_followup_nocough$stigma_score)
# result: all stigma_scores are not normally distributed -> perform wilcox test

####################################  Statistical Tests ####################################   
##############  case status
wilcox.test(datatb1_followup_case_completed$stigma_score, datatb2_followup_case_completed$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_case_cured$stigma_score, datatb2_followup_case_cured$stigma_score) #p-val < 0.01
summary(datatb1_followup_case_completed$stigma_score) #mean=15
summary(datatb2_followup_case_completed$stigma_score) #mean=12.63
summary(datatb1_followup_case_cured$stigma_score) #mean=16.11
summary(datatb2_followup_case_cured$stigma_score) #mean=14.36

##############  smoking status
wilcox.test(datatb1_followup_smoker$stigma_score, datatb2_followup_smoker$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_nonsmoker$stigma_score, datatb2_followup_nonsmoker$stigma_score) #p-val < 0.01
summary(datatb1_followup_smoker$stigma_score) #mean=15.84
summary(datatb2_followup_smoker$stigma_score) #mean=13.27
summary(datatb1_followup_nonsmoker$stigma_score) #mean=15.01
summary(datatb2_followup_nonsmoker$stigma_score) #mean=13.16

##############  cough symptom
wilcox.test(datatb1_followup_cough$stigma_score, datatb2_followup_cough$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_nocough$stigma_score, datatb2_followup_nocough$stigma_score) #p-val < 0.01
summary(datatb1_followup_cough$stigma_score) #mean=15.81
summary(datatb2_followup_cough$stigma_score) #mean=13.86
summary(datatb1_followup_nocough$stigma_score) #mean=14.04
summary(datatb2_followup_nocough$stigma_score) #mean=11.23
