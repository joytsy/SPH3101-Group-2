# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

##################################################
## Subgroup Analysis on stigma_scores for Fever ##
##################################################

# -------------- Subgroup which EXHIBITS Fever Symptom at baseline ------------------------
# Obtain Datasets
fever_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___5 %in% 1)
fever_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% fever_baseline$a1_record_id, ]
# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_fever <- inner_join(
  fever_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  fever_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns in paired_data_fever for better clarity
colnames(paired_data_fever) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup using paired_data_fever
summary(paired_data_fever$Baseline)  # mean = 16.63 at Baseline
summary(paired_data_fever$Followup)  # mean = 15.38 at Followup

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_fever$Baseline)  # p-value < 0.01 --> not normally distributed at Baseline
shapiro.test(paired_data_fever$Followup)  # p-value < 0.01 --> not normally distributed at Followup

# Statistical Tests
# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_fever$Baseline, paired_data_fever$Followup, paired = TRUE)  # p-value < 0.01


# ---------------- Subgroup which DID NOT EXHIBIT Fever Symptom at baseline --------------------
# Obtain Datasets
no_fever_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___5 %in% 0)
no_fever_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% no_fever_baseline$a1_record_id, ]
# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_no_fever <- inner_join(
  no_fever_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  no_fever_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns in paired_data_fever for better clarity
colnames(paired_data_no_fever) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup
summary(paired_data_no_fever$Baseline) # mean = 14.79
summary(paired_data_no_fever$Followup) # mean = 12.21

# Normality Tests
shapiro.test(paired_data_no_fever$Baseline) # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_fever$Followup) # p-value < 0.01 --> data is likely not normally distributed

# Statistical Tests
# Since the data is not normally distributed, use Wilcoxon's Test to compare stigma scores across time
wilcox.test(paired_data_no_fever$Baseline, paired_data_no_fever$Followup, paired=TRUE) # p-value < 0.01

