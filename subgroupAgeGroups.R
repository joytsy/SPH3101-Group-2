# Load Datasets, Run data-processing function source('load_data.r')
source('load_data.R')
source('MyFunctions.R')
source('processBeforeSubgroup.R')

##############################################################
## Subgroup Analysis on stigma_scores for <55 and >=55 y.o. ##
##############################################################

## ---------------- Ages <55 ----------------
# get dataset
baseline_below55 <- baseline_followup_complete_oldYoung[baseline_followup_complete_oldYoung$older=="No",]

followup_below55 <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_below55$a1_record_id, ]

# summary
summary(baseline_below55$stigma_score)
summary(followup_below55$stigma_score)

# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_below55$stigma_score)
shapiro.test(followup_below55$stigma_score)

# Statistical Tests
# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_below55$stigma_score, followup_below55$stigma_score, paired=TRUE)


## ---------------- Ages >=55 ----------------
# get dataset
baseline_55AndAbove <- baseline_followup_complete_oldYoung[baseline_followup_complete_oldYoung$older=="Yes",]

followup_55AndAbove <- datatb2_followup[datatb2_followup$a1_record_id %in% baseline_55AndAbove$a1_record_id, ]

# summary
summary(baseline_55AndAbove$stigma_score)
summary(followup_55AndAbove$stigma_score)

# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(baseline_55AndAbove$stigma_score)
shapiro.test(followup_55AndAbove$stigma_score)

# Statistical Tests
# Since stigma scores likely not normally distributed, use wilcox to compare the sample
wilcox.test(baseline_55AndAbove$stigma_score, followup_55AndAbove$stigma_score, paired=TRUE)
