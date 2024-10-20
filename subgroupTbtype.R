# Load Datasets, Run data-processing function source('load_data.r')
source('load_data.R')
source('MyFunctions.R')
source('processBeforeSubgroup.R')

##################################################
## Subgroup Analysis (TB type) on stigma_scores ##
##################################################

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


#  Statistical Tests
wilcox.test(datatb1_followup_TBbacPlus$stigma_score, datatb2_followup_TBbacPlus$stigma_score, paired=TRUE) #p-val < 0.01
wilcox.test(datatb1_followup_TBbacMinus$stigma_score, datatb2_followup_TBbacMinus$stigma_score, paired=TRUE) #p-val < 0.01
summary(datatb1_followup_TBbacPlus$stigma_score) #mean=16.11
summary(datatb2_followup_TBbacPlus$stigma_score) #mean=14.41
summary(datatb1_followup_TBbacMinus$stigma_score) #mean=15.05
summary(datatb2_followup_TBbacMinus$stigma_score) #mean=12.8


