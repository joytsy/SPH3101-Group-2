# Load Datasets, Run data-processing function source('load_data.r')
source('load_data.R')
source('MyFunctions.R')
source('processBeforeSubgroup.R')

#########################################################
## Subgroup Analysis (Smoking status) on stigma_scores ##
#########################################################

############## Obtain subgroup datasets from datatb1 and datatb2 in processBeforeSubgroup script ###########################

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

# Statistical Tests
wilcox.test(datatb1_followup_smoker$stigma_score, datatb2_followup_smoker$stigma_score, paired=TRUE) #p-val < 0.01
wilcox.test(datatb1_followup_nonsmoker$stigma_score, datatb2_followup_nonsmoker$stigma_score, paired=TRUE) #p-val < 0.01
summary(datatb1_followup_smoker$stigma_score) #mean=15.84
summary(datatb2_followup_smoker$stigma_score) #mean=13.27
summary(datatb1_followup_nonsmoker$stigma_score) #mean=15.01
summary(datatb2_followup_nonsmoker$stigma_score) #mean=13.16
