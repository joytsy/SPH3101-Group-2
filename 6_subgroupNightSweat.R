# Load Datasets, Run data-processing function source('load_data.r')
source('1_load_data.R')
source('2_MyFunctions.R')
source('2_processBeforeSubgroup.R')

########################################################
## Subgroup Analysis on stigma_scores for Night Sweat ##
########################################################

# -------------- Subgroup which EXPERIENCED Night Sweat Symptom at baseline ----------------
# Obtain Datasets
night_sweat_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___8 %in% 1)
night_sweat_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% night_sweat_baseline$a1_record_id, ]

# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_night_sweat <- inner_join(
  night_sweat_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  night_sweat_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns for better clarity
colnames(paired_data_night_sweat) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup using paired_data_night_sweat
summary(paired_data_night_sweat$Baseline)  # mean = 16.86
summary(paired_data_night_sweat$Followup)  # mean = 16.14


# Normality Tests for Baseline and Followup
shapiro.test(paired_data_night_sweat$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_night_sweat$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_night_sweat$Baseline, paired_data_night_sweat$Followup, paired = TRUE)  # p-value < 0.01


# -------------- Subgroup which EXPERIENCED Night Sweat Symptom at baseline ----------------
# Obtain Datasets
no_night_sweat_baseline <- baseline_followup_complete %>% 
  filter(a1_q28___8 %in% 0)
no_night_sweat_followup <- datatb2_followup[datatb2_followup$a1_record_id %in% no_night_sweat_baseline$a1_record_id, ]

# Create a dataframe combining baseline and follow-up stigma scores, properly matched by 'a1_record_id'
paired_data_no_night_sweat <- inner_join(
  no_night_sweat_baseline %>% select(a1_record_id, stigma_score) %>% rename(baseline_score = stigma_score),
  no_night_sweat_followup %>% select(a1_record_id, stigma_score) %>% rename(followup_score = stigma_score),
  by = "a1_record_id"
)
# Rename the columns for better clarity
colnames(paired_data_no_night_sweat) <- c("record_id", "Baseline", "Followup")

# Summary Statistics for Baseline and Followup using paired_data_no_night_sweat
summary(paired_data_no_night_sweat$Baseline)  # mean = 14.78
summary(paired_data_no_night_sweat$Followup)  # mean = 12.05


# Normality Tests for Baseline and Followup
shapiro.test(paired_data_no_night_sweat$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_night_sweat$Followup)  # p-value < 0.01 --> data is likely not normally distributed


# Statistical Tests
# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_no_night_sweat$Baseline, paired_data_no_night_sweat$Followup, paired = TRUE)  # p-value < 0.05

