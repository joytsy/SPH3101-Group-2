# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

############################################################
## Hypothesis testing for 621 individuals who followed up ##
############################################################

# Shapiro-Wilk test to test for normality in both baseline and follow up cohorts of 621 individuals
# baseline: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)])
# follow-up: p-value < 0.01 --> data is likely not normally distributed
shapiro.test(datatb2$stigma_score[!is.na(datatb2$stigma_score)])

# Since not normally distributed, use wilcoxon to compare the sample of 621 at different time frames
wilcox.test(datatb1$stigma_score[!is.na(datatb2$stigma_score)],datatb2$stigma_score[!is.na(datatb2$stigma_score)] , paired=TRUE)
