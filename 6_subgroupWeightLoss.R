# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

########################################################
## Subgroup Analysis on stigma_scores for Weight Loss ##
########################################################

# -------------- Subgroup which EXPERIENCED Weight Loss Symptom at baseline ----------------
# Obtain Datasets
weight_loss_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___7 %in% 1)
weight_loss_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% weight_loss_baseline$a1_record_id, ]

# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_weight_loss <- inner_join(
  weight_loss_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  weight_loss_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns in paired_data_weight_loss for better clarity
colnames(paired_data_weight_loss) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup using paired_data_weight_loss
summary(paired_data_weight_loss$Baseline)  # mean = 16.64
summary(paired_data_weight_loss$Followup)  # mean = 16.07


# Normality Tests for Baseline and Followup
shapiro.test(paired_data_weight_loss$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_weight_loss$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Statistical Tests
# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_weight_loss$Baseline, paired_data_weight_loss$Followup, paired = TRUE)  # 0.01 < p-value (0.03339) < 0.05



# -------------- Subgroup which DID NOT EXPERIENCE Weight Loss Symptom at baseline ------------
# Obtain Datasets
no_weight_loss_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___7 %in% 0)
no_weight_loss_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% no_weight_loss_baseline$a1_record_id, ]

# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_no_weight_loss <- inner_join(
  no_weight_loss_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  no_weight_loss_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns in paired_data_no_weight_loss for better clarity
colnames(paired_data_no_weight_loss) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup using paired_data_no_weight_loss
summary(paired_data_no_weight_loss$Baseline)  # mean = 15.01
summary(paired_data_no_weight_loss$Followup)  # mean = 12.39


# Normality Tests for Baseline and Followup
shapiro.test(paired_data_no_weight_loss$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_weight_loss$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Statistical Tests
# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_no_weight_loss$Baseline, paired_data_no_weight_loss$Followup, paired = TRUE)  # p-value < 0.05

