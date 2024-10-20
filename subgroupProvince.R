# Load Datasets, Run data-processing function source('load_data.r')
source('load_data.R')
source('MyFunctions.R')
source('processBeforeSubgroup.R')

###################################################
## Subgroup Analysis (Province) on stigma_scores ##
###################################################
# correlation between province and operational district
test <- table(as_factor(datatb1$a1_operat_dist), as_factor(datatb1$a1_prov))
chisq.test(test) # statistically significant --> p-value < 0.01

# head(test)

## ---------------- Kampong Cham ----------------
# get dataset
kampCham_baseline <- baseline_followup_complete %>%
  filter(a1_prov==1) # filter for Kampong Cham (label=1)

kampCham_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% kampCham_baseline$a1_record_id,]

# summary
summary(kampCham_baseline$stigma_score)
summary(kampCham_followup$stigma_score)


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(kampCham_baseline$stigma_score)
shapiro.test(kampCham_followup$stigma_score)

# Statistical Tests
# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(kampCham_baseline$stigma_score, kampCham_followup$stigma_score, paired=TRUE)


## ---------------- Tboung Khmum ----------------
# get dataset
tboung_baseline <- baseline_followup_complete %>%
  filter(a1_prov==2) # filter for Tboung Khmum (label=2)

tboung_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% tboung_baseline$a1_record_id,]

# summary
summary(tboung_baseline$stigma_score)
summary(tboung_followup$stigma_score)


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(tboung_baseline$stigma_score)
shapiro.test(tboung_followup$stigma_score)

# Statistical Tests
# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(tboung_baseline$stigma_score, tboung_followup$stigma_score, paired=TRUE)


## ---------------- Kandal ----------------
# get dataset
kandal_baseline <- baseline_followup_complete %>%
  filter(a1_prov==3) # filter for Kandal (label=3)

kandal_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% kandal_baseline$a1_record_id, ]

# summary
summary(kandal_baseline$stigma_score)
summary(kandal_followup$stigma_score)


# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(kandal_baseline$stigma_score)
shapiro.test(kandal_followup$stigma_score)

# Statistical Tests
# Since not normally distributed, use wilcoxon to compare the sample at different time frames
wilcox.test(kandal_baseline$stigma_score, kandal_followup$stigma_score, paired=TRUE)



## ---------------- Phnom Penh ----------------
# get dataset
phnom_baseline <- baseline_followup_complete %>%
  filter(a1_prov==4) # filter for Phnom Penh (label=4)

phnom_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% phnom_baseline$a1_record_id, ]

# summary
summary(phnom_baseline$stigma_score)
summary(phnom_followup$stigma_score)

# Shapiro-Wilk test to test for normality in both baseline and follow up
shapiro.test(phnom_baseline$stigma_score)
shapiro.test(phnom_followup$stigma_score)

# Statistical Tests
# Since stigma scores in both time frames likely normally distributed, use t-test to compare the sample
t.test(phnom_baseline$stigma_score, phnom_followup$stigma_score, paired=TRUE)






