# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

#####################################################
## Subgroup Analysis (chest pain) on stigma_scores ##
#####################################################

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


# Statistical Tests
wilcox.test(datatb1_followup_chestpain$stigma_score, datatb2_followup_chestpain$stigma_score, paired=TRUE) #p-val =  0.02049
wilcox.test(datatb1_followup_nochestpain$stigma_score, datatb2_followup_nochestpain$stigma_score, paired=TRUE) #p-val < 0.01
summary(datatb1_followup_chestpain$stigma_score) #mean=16.63
summary(datatb2_followup_chestpain$stigma_score) #mean=15.45
summary(datatb1_followup_nochestpain$stigma_score) #mean=15.07
summary(datatb2_followup_nochestpain$stigma_score) #mean=12.67