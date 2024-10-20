# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

######################################################
## Subgroup Analysis (Case status) on stigma_scores ##
######################################################

############## Obtain subgroup datasets from datatb1 and datatb2 in processBeforeSubgroup script ###########################
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

# Statistical Tests
wilcox.test(datatb1_followup_case_completed$stigma_score, datatb2_followup_case_completed$stigma_score, paired=TRUE) #p-val < 0.01
wilcox.test(datatb1_followup_case_cured$stigma_score, datatb2_followup_case_cured$stigma_score, paired=TRUE) #p-val < 0.01
summary(datatb1_followup_case_completed$stigma_score) #mean=15
summary(datatb2_followup_case_completed$stigma_score) #mean=12.63
summary(datatb1_followup_case_cured$stigma_score) #mean=16.11
summary(datatb2_followup_case_cured$stigma_score) #mean=14.36