# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

################################################
## Subgroup Analysis (Cough) on stigma_scores ##
################################################

############## Obtain subgroup datasets from datatb1 and datatb2 in processBeforeSubgroup script ###########################

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


# Statistical Tests
wilcox.test(datatb1_followup_cough$stigma_score, datatb2_followup_cough$stigma_score, paired=TRUE) #p-val < 0.01
wilcox.test(datatb1_followup_nocough$stigma_score, datatb2_followup_nocough$stigma_score, paired=TRUE) #p-val < 0.01
summary(datatb1_followup_cough$stigma_score) #mean=15.81
summary(datatb2_followup_cough$stigma_score) #mean=13.86
summary(datatb1_followup_nocough$stigma_score) #mean=14.04
summary(datatb2_followup_nocough$stigma_score) #mean=11.23
