# ++++++++ Stigma EDA ++++++++ 

# Run 'load_data.r' script to load datatb1 dataset, else can run the following line
# load('data/datatb1.rdata')
# load('data/datatb2.rdata')

# -------- Load Libraries --------
# Load package ggplot2 and dplyr(to be used for data manipulation and data visualisation)
library(ggplot2)
library(dplyr)
library(haven)

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
# remove observations where individuals < 18 yo
datatb2 <- datatb2 %>%
  filter(a1_record_id %in% datatb1$a1_record_id)

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

# Shapiro-Wilk test to test for normality in both baseline and follow up cohorts of 621 individuals
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)])
# follow-up: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb2$stigma_score[!is.na(datatb2$stigma_score)])

# Since not normally distributed, use wilcoxon to compare the sample of 621 at different time frames
wilcox.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)] , paired=TRUE)
boxplot(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)], names = c("Baseline", "Follow up"),
        main = "Boxplot of Stigma Scores of Follow Up Cohort at Baseline and Follow-Up")

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
boxplot(datatb1_followup_case_completed$stigma_score,datatb2_followup_case_completed$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of Case Status: Completed Cohort at Baseline and Follow-Up")
boxplot(datatb1_followup_case_cured$stigma_score,datatb2_followup_case_cured$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of Case Status: Cured Cohort at Baseline and Follow-Up")

##############  smoking status
wilcox.test(datatb1_followup_smoker$stigma_score, datatb2_followup_smoker$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_nonsmoker$stigma_score, datatb2_followup_nonsmoker$stigma_score) #p-val < 0.01
summary(datatb1_followup_smoker$stigma_score) #mean=15.84
summary(datatb2_followup_smoker$stigma_score) #mean=13.27
summary(datatb1_followup_nonsmoker$stigma_score) #mean=15.01
summary(datatb2_followup_nonsmoker$stigma_score) #mean=13.16
boxplot(datatb1_followup_smoker$stigma_score,datatb2_followup_smoker$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of Smoker Cohort at Baseline and Follow-Up")
boxplot(datatb1_followup_nonsmoker$stigma_score,datatb2_followup_nonsmoker$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of Non-smoker Cohort at Baseline and Follow-Up")

##############  cough symptom
wilcox.test(datatb1_followup_cough$stigma_score, datatb2_followup_cough$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_nocough$stigma_score, datatb2_followup_nocough$stigma_score) #p-val < 0.01
summary(datatb1_followup_cough$stigma_score) #mean=15.81
summary(datatb2_followup_cough$stigma_score) #mean=13.86
summary(datatb1_followup_nocough$stigma_score) #mean=14.04
summary(datatb2_followup_nocough$stigma_score) #mean=11.23
boxplot(datatb1_followup_cough$stigma_score,datatb2_followup_cough$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of with Cough Cohort at Baseline and Follow-Up")
boxplot(datatb1_followup_nocough$stigma_score,datatb2_followup_nocough$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of No cough Cohort at Baseline and Follow-Up")







##############################################################
## Subgroup Analysis (TB type, chest pain) on stigma_scores ##
##############################################################

##############  chest pain symptom
# create new column & copy over cough "a1_q28___3" for those who followed-up in datatb2_followup from datatb1
datatb2_followup <- datatb2_followup %>%
  mutate(chest_pain_copy = datatb1$a1_q28___3[datatb1$a1_record_id %in% datatb2_followup$a1_record_id])
# get datasets
datatb2_followup_chestpain <- datatb2_followup %>%
  filter(datatb2_followup$chest_pain_copy == 1)
datatb2_followup_nochestpain <- datatb2_followup %>%
  filter(datatb2_followup$chest_pain_copy == 0)
datatb1_followup_chestpain <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_chestpain$a1_record_id)
datatb1_followup_nochestpain <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_nochestpain$a1_record_id)

# Shapiro-Wilk test to test for normality
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1_followup_chestpain$stigma_score)
shapiro.test(datatb1_followup_nochestpain$stigma_score)
shapiro.test(datatb2_followup_chestpain$stigma_score)
shapiro.test(datatb2_followup_nochestpain$stigma_score)
# result: all stigma_scores are not normally distributed -> perform wilcox test

##############  TB Type 
# TB Type in datatb2_followup "a1_type_tb_fu" tallies with baseline_followup_complete "a1_type_tb"
table(datatb2_followup$a1_type_tb_fu)
table(baseline_followup_complete$a1_type_tb)
## status 4 does not have a sample size that is large enough for statistical tests to be peformed

# get datasets
datatb2_followup_TBbacPlus <- datatb2_followup %>%
  filter(datatb2_followup$a1_type_tb_fu == 1)
datatb2_followup_TBbacMinus <- datatb2_followup %>%
  filter(datatb2_followup$a1_type_tb_fu == 2)
datatb2_followup_RRTB <- datatb2_followup %>%
  filter(datatb2_followup$a1_type_tb_fu == 4)

datatb1_followup_TBbacPlus <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_TBbacPlus$a1_record_id)
datatb1_followup_TBbacMinus <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_TBbacMinus$a1_record_id)
datatb1_followup_RRTB <- datatb1 %>%
  filter(datatb1$a1_record_id %in% datatb2_followup_RRTB$a1_record_id)

# Shapiro-Wilk test to test for normality
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1_followup_TBbacPlus$stigma_score) #p-value < 0.01
shapiro.test(datatb1_followup_TBbacMinus$stigma_score) #p-value < 0.01
shapiro.test(datatb2_followup_TBbacPlus$stigma_score) #p-value < 0.01
shapiro.test(datatb2_followup_TBbacMinus$stigma_score) #p-value < 0.01
# result: For TBbacPlus,TBbacMinus stigma_scores NOT normally distributed -> perform wilcox test



####################################  Statistical Tests #################################### 

##############  chest pain symptom
wilcox.test(datatb1_followup_chestpain$stigma_score, datatb2_followup_chestpain$stigma_score) #p-val =  0.04902
wilcox.test(datatb1_followup_nochestpain$stigma_score, datatb2_followup_nochestpain$stigma_score) #p-val < 0.01
summary(datatb1_followup_chestpain$stigma_score) #mean=16.63
summary(datatb2_followup_chestpain$stigma_score) #mean=15.45
summary(datatb1_followup_nochestpain$stigma_score) #mean=15.07
summary(datatb2_followup_nochestpain$stigma_score) #mean=12.67
boxplot(datatb1_followup_chestpain$stigma_score,datatb2_followup_chestpain$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of with Chest pain Cohort at Baseline and Follow-Up")
boxplot(datatb1_followup_nochestpain$stigma_score,datatb2_followup_nochestpain$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of No Chest pain Cohort at Baseline and Follow-Up")


##############  TB Type
wilcox.test(datatb1_followup_TBbacPlus$stigma_score, datatb2_followup_TBbacPlus$stigma_score) #p-val < 0.01
wilcox.test(datatb1_followup_TBbacMinus$stigma_score, datatb2_followup_TBbacMinus$stigma_score) #p-val < 0.01
summary(datatb1_followup_TBbacPlus$stigma_score) #mean=16.11
summary(datatb2_followup_TBbacPlus$stigma_score) #mean=14.41
summary(datatb1_followup_TBbacMinus$stigma_score) #mean=15.05
summary(datatb2_followup_TBbacMinus$stigma_score) #mean=12.8
boxplot(datatb1_followup_TBbacPlus$stigma_score,datatb2_followup_TBbacPlus$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of TB Bac- Cohort at Baseline and Follow-Up")
boxplot(datatb1_followup_TBbacMinus$stigma_score,datatb2_followup_TBbacMinus$stigma_score, names = c("Baseline", "Follow-up"),
        main = "Boxplot of Stigma Scores of TB Bac- Cohort at Baseline and Follow-Up")
