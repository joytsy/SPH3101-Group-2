#####################################################################################
## PRECONDITIONS: Ran 'processBeforeSubgroup.R' or relevant code which:
## - Loaded required libraries
## - Loaded both DFs: datatb2_followup and baseline_followup_complete ##
#####################################################################################

# ------------------------------------ FEVER ------------------------------------- ##
#####################################################################################
## Subgroup Analysis on stigma_scores over time: EXHIBITS FEVER SYMPTOM ---------- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_fever, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Had Fever (Baseline)", x = "Stigma Score", y = "Frequency")

ggplot(paired_data_fever, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Had Fever (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_fever$Baseline)  # p-value < 0.01 --> not normally distributed at Baseline
shapiro.test(paired_data_fever$Followup)  # p-value < 0.01 --> not normally distributed at Followup

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_fever$Baseline, paired_data_fever$Followup, paired = TRUE)  # p-value < 0.01

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_fever$Baseline, paired_data_fever$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which experienced Fever")

# ADDITIONAL: Create a dataframe in long format
paired_data_fever_long <- paired_data_fever %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_fever_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (Fever Subgroup)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if too crowded





#####################################################################################
## Subgroup Analysis on stigma_scores over time: DID NOT EXHIBIT FEVER SYMPTOM --- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_no_fever, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - No Fever (Baseline)", x = "Stigma Score", y = "Frequency")
ggplot(paired_data_no_fever, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - No Fever (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests
shapiro.test(paired_data_no_fever$Baseline) # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_fever$Followup) # p-value < 0.01 --> data is likely not normally distributed

# Since the data is not normally distributed, use Wilcoxon's Test to compare stigma scores across time
wilcox.test(paired_data_no_fever$Baseline, paired_data_no_fever$Followup, paired=TRUE) # p-value < 0.01

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_no_fever$Baseline, paired_data_no_fever$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which Did Not Have Fever")

# ADDITIONAL: Create a dataframe in long format
paired_data_no_fever_long <- paired_data_no_fever %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_no_fever_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (Subgroup: No Fever)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if too crowded




# ---------------------------------- WEIGHT LOSS --------------------------------- ##
#####################################################################################
## Subgroup Analysis on stigma_scores over time - EXHIBIT WEIGHT LOSS SYMPTOM ---- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_weight_loss, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Experienced Weight Loss (Baseline)", x = "Stigma Score", y = "Frequency")

ggplot(paired_data_weight_loss, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Experienced Weight Loss (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_weight_loss$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_weight_loss$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_weight_loss$Baseline, paired_data_weight_loss$Followup, paired = TRUE)  # 0.01 < p-value (0.03339) < 0.05

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_weight_loss$Baseline, paired_data_weight_loss$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which experienced Weight Loss")

# ADDITIONAL: Create a dataframe in long format
paired_data_weight_loss_long <- paired_data_weight_loss %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_weight_loss_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (Weight Loss Subgroup)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if too crowded






#####################################################################################
## Subgroup Analysis on stigma_scores over time - DID NOT EXHIBIT WEIGHT LOSS ---- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_no_weight_loss, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Did Not Experience Weight Loss (Baseline)", x = "Stigma Score", y = "Frequency")

ggplot(paired_data_no_weight_loss, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Did Not Experience Weight Loss (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_no_weight_loss$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_weight_loss$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_no_weight_loss$Baseline, paired_data_no_weight_loss$Followup, paired = TRUE)  # p-value < 0.05

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_no_weight_loss$Baseline, paired_data_no_weight_loss$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which did not experience Weight Loss")

# ADDITIONAL: Create a dataframe in long format
paired_data_no_weight_loss_long <- paired_data_no_weight_loss %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_no_weight_loss_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (No Weight Loss Subgroup)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")






# ---------------------------------- NIGHT SWEAT --------------------------------- ##
#####################################################################################
## Subgroup Analysis on stigma_scores over time - EXHIBIT NIGHT SWEAT SYMPTOM ---- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_night_sweat, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Experienced Night Sweat (Baseline)", x = "Stigma Score", y = "Frequency")

ggplot(paired_data_night_sweat, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Experienced Night Sweat (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_night_sweat$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_night_sweat$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_night_sweat$Baseline, paired_data_night_sweat$Followup, paired = TRUE)  # p-value < 0.01

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_night_sweat$Baseline, paired_data_night_sweat$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which experienced Night Sweat")

# ADDITIONAL: Create a dataframe in long format
paired_data_night_sweat_long <- paired_data_night_sweat %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_night_sweat_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (Night Sweat Subgroup)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")





#####################################################################################
## Subgroup Analysis on stigma_scores over time - DID NOT EXHIBIT NIGHT SWEAT ---- ##
#####################################################################################

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

# Histograms for Baseline and Followup
ggplot(paired_data_no_night_sweat, aes(x = Baseline)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Did Not Exhibit Night Sweat (Baseline)", x = "Stigma Score", y = "Frequency")

ggplot(paired_data_no_night_sweat, aes(x = Followup)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Stigma Scores - Did Not Exhibit Night Sweat (Followup)", x = "Stigma Score", y = "Frequency")

# Normality Tests for Baseline and Followup
shapiro.test(paired_data_no_night_sweat$Baseline)  # p-value < 0.01 --> data is likely not normally distributed
shapiro.test(paired_data_no_night_sweat$Followup)  # p-value < 0.01 --> data is likely not normally distributed

# Paired Wilcoxon Test (since data is not normally distributed)
wilcox.test(paired_data_no_night_sweat$Baseline, paired_data_no_night_sweat$Followup, paired = TRUE)  # p-value < 0.05

# Boxplot of Baseline and Followup Stigma Scores
boxplot(paired_data_no_night_sweat$Baseline, paired_data_no_night_sweat$Followup, 
        names = c("Baseline", "Followup"),
        main = "Boxplot of Stigma Scores of Subgroup which did not exhibit Night Sweat")

# ADDITIONAL: Create a dataframe in long format
paired_data_no_night_sweat_long <- paired_data_no_night_sweat %>%
  pivot_longer(cols = c(Baseline, Followup), 
               names_to = "Time", 
               values_to = "Stigma_Score")

# Paired Difference Plot (Connecting Line Plot)
ggplot(paired_data_no_night_sweat_long, aes(x = Time, y = Stigma_Score, group = record_id)) +
  geom_point() +
  geom_line(aes(color = record_id)) +
  labs(title = "Paired Difference Plot for Stigma Scores (No Night Sweat Subgroup)",
       x = "Time Point", y = "Stigma Score") +
  theme_minimal() +
  theme(legend.position = "none")
